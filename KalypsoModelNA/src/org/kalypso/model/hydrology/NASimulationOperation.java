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
import org.kalypso.model.hydrology.internal.NaSimulationDirs;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.simulation.NaModelInnerCalcJob;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.KalypsoSimulationCoreDebug;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.refactoring.local.LocalSimulationMonitor;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.commons.StatusCollection;

/**
 * Runs a simulation directly from a workspace folder (calc case).
 * 
 * @author Gernot Belger
 */
public class NASimulationOperation implements ICoreRunnableWithProgress
{
  /**
   * The rrm simulation.
   */
  private final RrmSimulation m_simulation;

  /**
   * The simulation data.
   */
  private final INaSimulationData m_simulationData;

  /**
   * The constructor.
   * 
   * @param simulationFolder
   *          The folder of the simulation
   * @param simulationData
   *          The simulation data.
   */
  public NASimulationOperation( final IFolder simulationFolder, final INaSimulationData simulationData )
  {
    m_simulation = new RrmSimulation( simulationFolder );
    m_simulationData = simulationData;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    /* If one simulation data object was provided, we will use this one. */
    /* Otherwise we load a new one. */
    INaSimulationData data = m_simulationData;
    if( data == null )
      data = loadData();

    /* Create the temporary directory for the simulation. */
    final File simulationTmpDir = FileUtilities.createNewTempDir( "naSimulation" ); //$NON-NLS-1$

    /* The calc job which calculates the simulation. */
    NaModelInnerCalcJob calcJob = null;

    try
    {
      /* Create the calc job. */
      calcJob = new NaModelInnerCalcJob( data, simulationTmpDir );

      /* A simulation monitor. */
      final ISimulationMonitor simulationMonitor = new LocalSimulationMonitor( monitor );

      /* Calculate the simulation. */
      calcJob.run( simulationMonitor );

      /* Load the simulation log. */
      /* If one is available, it should contain all messages. */
      final IStatus simulationLog = loadSimulationLog( simulationTmpDir );
      if( simulationLog != null )
        return simulationLog;

      /* If not, there was an error and the log could not be written. */
      /* So we take the message of the simulation monitor. */
      final int finishSeverity = simulationMonitor.getFinishStatus();
      final String finishMessage = simulationMonitor.getFinishText();
      if( StringUtils.isBlank( finishMessage ) )
        return Status.OK_STATUS;

      return new Status( finishSeverity, ModelNA.PLUGIN_ID, finishMessage );
    }
    catch( final SimulationException e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, Messages.getString("NASimulationOperation_0"), e ); //$NON-NLS-1$
    }
    finally
    {
      if( calcJob != null )
        finalizeSimulation( calcJob, simulationTmpDir );
    }
  }

  /**
   * This function loads the simulation data.
   * 
   * @return The simulation data.
   */
  private INaSimulationData loadData( ) throws CoreException
  {
    try
    {
      final URL modelURL = ResourceUtilities.createURL( m_simulation.getModelGml() );
      final URL controlURL = ResourceUtilities.createURL( m_simulation.getExpertControlGml() );
      final URL metaURL = ResourceUtilities.createURL( m_simulation.getCalculationGml() );
      final URL parameterURL = ResourceUtilities.createURL( m_simulation.getParameterGml() );
      final URL hydrotopURL = ResourceUtilities.createURL( m_simulation.getHydrotopGml() );
      final URL syntNURL = ResourceUtilities.createURL( m_simulation.getSyntnGml() );
      final URL lzsimURL = ResourceUtilities.createURL( m_simulation.getLzsimGml() );

      return NaSimulationDataFactory.load( modelURL, controlURL, metaURL, parameterURL, hydrotopURL, syntNURL, lzsimURL, null, null, null, null );
    }
    catch( final MalformedURLException e )
    {
      throw new CoreException( new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, Messages.getString("NASimulationOperation_1"), e ) ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      throw new CoreException( new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, Messages.getString("NASimulationOperation_2"), e ) ); //$NON-NLS-1$
    }
  }

  /**
   * This function loads the simulation log. Afterwards it deletes the log file.
   * 
   * @param simulationTmpDir
   *          The temporary directory of the simulation.
   * @return The simulation log.
   */
  private IStatus loadSimulationLog( final File simulationTmpDir )
  {
    /* The simulation log. */
    File simulationLogFile = null;

    try
    {
      /* Get the simulation log file. */
      final File resultDir = new File( simulationTmpDir, "results/Ergebnisse/Aktuell" ); //$NON-NLS-1$
      final File logDir = new File( resultDir, "Log" ); //$NON-NLS-1$
      simulationLogFile = new File( logDir, "simulationLog.gml" ); //$NON-NLS-1$
      if( !simulationLogFile.exists() )
        return new Status( IStatus.INFO, ModelNA.PLUGIN_ID, Messages.getString("NASimulationOperation_6") ); //$NON-NLS-1$

      /* Load the workspace. */
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( simulationLogFile, null );

      /* Get the root feature. */
      final Feature rootFeature = workspace.getRootFeature();

      /* Cast to status collection. */
      final StatusCollection statusCollection = (StatusCollection) rootFeature;

      return statusCollection.toStatus();
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, Messages.getString("NASimulationOperation_7"), ex ); //$NON-NLS-1$
    }
    finally
    {
      if( simulationLogFile != null )
        FileUtils.deleteQuietly( simulationLogFile );
    }
  }

  /**
   * This function finalizes the simulation and deletes the temporary directory of the simulation, if debug is enabled.
   * 
   * @param calcJob
   *          The calc job which calculates the simulation.
   * @param simulationTmpDir
   *          The temporary directory of the simulation.
   */
  private void finalizeSimulation( final NaModelInnerCalcJob calcJob, final File simulationTmpDir )
  {
    try
    {
      /* Handle the results. */
      handleResults( calcJob.getResultDir() );
    }
    catch( final CoreException e )
    {
      // FIXME: better error handling
      ModelNA.getDefault().getLog().log( e.getStatus() );
    }

    try
    {
      /* Delete the temporary directory of the simulation, if debug is enabled. */
      if( !KalypsoSimulationCoreDebug.KEEP_SIMULATION_FILES.isEnabled() )
        FileUtils.forceDelete( simulationTmpDir );
    }
    catch( final IOException e )
    {
      final IStatus status = new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, Messages.getString("NASimulationOperation_8"), e ); //$NON-NLS-1$
      ModelNA.getDefault().getLog().log( status );
    }
  }

  /**
   * This function deletes the old results and moves the new results to the results directory. The new results will also
   * be copied to a directory with a timestamp.
   * 
   * @param resultDir
   *          The directory of the results.
   */
  private void handleResults( final File resultDir ) throws CoreException
  {
    /* Create a filename with the timestamp format. */
    final SimpleDateFormat timestampFormat = new SimpleDateFormat( "yyyy.MM.dd_(HH_mm_ss)" ); //$NON-NLS-1$
    final String timestampFilename = timestampFormat.format( new Date() );

    /* Get the current result directory. */
    final IFolder currentResultFolder = m_simulation.getCurrentCalculationResult().getFolder();
    final File currentResultDir = currentResultFolder.getLocation().toFile();

    /* Get the timestamp directory. */
    final IFolder calcResultFolder = m_simulation.getResultsFolder();
    final IFolder timestampFolder = calcResultFolder.getFolder( timestampFilename );
    final File timestampDir = timestampFolder.getLocation().toFile();

    try
    {
      /* Remove old 'Aktuell' dir. */
      if( currentResultDir.exists() )
        FileUtils.forceDelete( currentResultDir );

      /* Move new result to calc Folder: move is way faster... */
      FileUtils.moveDirectory( new File( resultDir, NaSimulationDirs.DIR_CURRENT_RESULT ), currentResultDir );

      /* Make copy with timestamp. */
      FileUtils.copyDirectory( currentResultDir, timestampDir );
    }
    catch( final IOException e )
    {
      throw new CoreException( new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, "Failed to copy result data", e ) ); //$NON-NLS-1$
    }
    finally
    {
      /* Refresh the folders. */
      currentResultFolder.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
      timestampFolder.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
    }
  }
}