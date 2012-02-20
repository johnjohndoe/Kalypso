/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.hydrology;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.simulation.NaModelInnerCalcJob;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.KalypsoSimulationCoreDebug;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.refactoring.local.LocalSimulationMonitor;

/**
 * Runs a simulation directly from a workspace folder (calc case).
 *
 * @author Gernot Belger
 */
public class NASimulationOperation implements ICoreRunnableWithProgress
{
  private final RrmSimulation m_calcCase;

  public NASimulationOperation( final IFolder calcCase )
  {
    m_calcCase = new RrmSimulation( calcCase );
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final INaSimulationData data = loadData();

    final File simulationDir = FileUtilities.createNewTempDir( "naSimulation" ); //$NON-NLS-1$

    NaModelInnerCalcJob calcJob = null;
    try
    {
      calcJob = new NaModelInnerCalcJob( data, simulationDir );

      final ISimulationMonitor simulationMonitor = new LocalSimulationMonitor( monitor );

      calcJob.run( simulationMonitor );

      final int finishSeverity = simulationMonitor.getFinishStatus();
      final String finishMessage = simulationMonitor.getFinishText();
      if( StringUtils.isBlank( finishMessage ) )
        return Status.OK_STATUS;

      return new Status( finishSeverity, ModelNA.PLUGIN_ID, finishMessage );
    }
    catch( final SimulationException e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, "Simulation failed", e );
    }
    finally
    {
      if( calcJob != null )
        finalizeSimulation( calcJob, simulationDir );
    }
  }

  private void finalizeSimulation( final NaModelInnerCalcJob calcJob, final File simulationDir )
  {
    try
    {
      handleResults( calcJob.getResultDir() );
    }
    catch( final CoreException e )
    {
      // FIXME: better error handling
      ModelNA.getDefault().getLog().log( e.getStatus() );
    }

    try
    {

      if( !KalypsoSimulationCoreDebug.KEEP_SIMULATION_FILES.isEnabled() )
        FileUtils.forceDelete( simulationDir );
    }
    catch( final IOException e )
    {
      final IStatus status = new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, "Failed to delete temporary simulation directory", e );
      ModelNA.getDefault().getLog().log( status );
    }
  }

  private void handleResults( final File resultDir ) throws CoreException
  {
    final IFolder calcResultFolder = m_calcCase.getResultsFolder();
    final IFolder currentResultFolder = m_calcCase.getCurrentResultsFolder();

    final SimpleDateFormat timestampFormat = new SimpleDateFormat( "yyyy.MM.dd_(HH_mm_ss)" );
    final String timestampFilename = timestampFormat.format( new Date() );
    final IFolder timestampFolder = calcResultFolder.getFolder( timestampFilename );

    final File currentResultDir = currentResultFolder.getLocation().toFile();
    final File timestampDir = timestampFolder.getLocation().toFile();

    try
    {
      /* Remove old 'Aktuell' dir */
      if( currentResultDir.exists() )
        FileUtils.forceDelete( currentResultDir );

      /* move new result to calc Folder: move is way faster... */
      FileUtils.moveDirectory( new File( resultDir, "Aktuell" ), currentResultDir );

      /* make copy with timestamp */
      FileUtils.copyDirectory( currentResultDir, timestampDir );
    }
    catch( final IOException e )
    {
      final IStatus status = new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, "Failed to copy result data", e );
      throw new CoreException( status );
    }
    finally
    {
      currentResultFolder.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
      timestampFolder.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
    }
  }

  private INaSimulationData loadData( ) throws CoreException
  {
    try
    {
      final URL modelURL = ResourceUtilities.createURL( m_calcCase.getModelGml() );
      final URL controlURL = ResourceUtilities.createURL( m_calcCase.getExpertControlGml() );
      final URL metaURL = ResourceUtilities.createURL( m_calcCase.getCalculationGml() );
      final URL parameterURL = ResourceUtilities.createURL( m_calcCase.getParameterGml() );
      final URL hydrotopURL = ResourceUtilities.createURL( m_calcCase.getHydrotopGml() );
      final URL sudsURL = ResourceUtilities.createURL( m_calcCase.getSudsGml() );
      final URL syntNURL = ResourceUtilities.createURL( m_calcCase.getSyntnGml() );
      final URL lzsimURL = ResourceUtilities.createURL( m_calcCase.getLzsimGml() );

      return NaSimulationDataFactory.load( modelURL, controlURL, metaURL, parameterURL, hydrotopURL, sudsURL, syntNURL, lzsimURL, null, null );
    }
    catch( final MalformedURLException e )
    {
      final Status status = new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, "Failed to access model file", e );
      throw new CoreException( status );
    }
    catch( final Exception e )
    {
      final Status status = new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, "Failed to load model data", e );
      throw new CoreException( status );
    }
  }
}
