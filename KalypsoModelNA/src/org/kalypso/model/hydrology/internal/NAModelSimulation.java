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
package org.kalypso.model.hydrology.internal;

import java.io.File;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollectorWithTime;
import org.kalypso.kalypsosimulationmodel.ui.calccore.CalcCoreUtils;
import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.binding.NAOptimize;
import org.kalypso.model.hydrology.binding.control.NAModellControl;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.postprocessing.NaPostProcessingException;
import org.kalypso.model.hydrology.internal.postprocessing.NaPostProcessor;
import org.kalypso.model.hydrology.internal.preprocessing.NAModelPreprocessor;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.ICatchmentInfos;
import org.kalypso.model.hydrology.internal.processing.KalypsoNaProcessor;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.osgi.framework.Version;

/**
 * @author Gernot Belger
 */
public class NAModelSimulation
{
  private static final String EXECUTABLES_FILE_TEMPLATE = "Kalypso-NA_%s.exe"; //$NON-NLS-1$

  static final String EXECUTABLES_FILE_PATTERN = "Kalypso-NA_(.+)\\.exe"; //$NON-NLS-1$

  private final NaSimulationDirs m_simDirs;

  private final INaSimulationData m_data;

  private NAModelPreprocessor m_preprocessor;

  private KalypsoNaProcessor m_processor;

  public NAModelSimulation( final NaSimulationDirs simDirs, final INaSimulationData data )
  {
    m_simDirs = simDirs;
    m_data = data;
  }

  public IStatus runSimulation( final ISimulationMonitor monitor )
  {
    /* The status collector. */
    final IStatusCollector collector = new StatusCollectorWithTime( ModelNA.PLUGIN_ID );

    try
    {
      /* Monitor. */
      monitor.setMessage( Messages.getString( "NAModelSimulation.3" ) ); //$NON-NLS-1$

      /* determine calculation core */
      final File naExe = determineCalculationCore( collector );
      final Version calcCoreVersion = findCalcCoreVersion( naExe.getName() );
      validateVersion( calcCoreVersion, collector );

      /* Pre processing. */
      final IStatus preProcessStatus = preProcess( m_data, calcCoreVersion, monitor );
      if( !preProcessStatus.isOK() )
        collector.add( preProcessStatus );

      if( monitor.isCanceled() )
      {
        collector.add( new Status( IStatus.CANCEL, ModelNA.PLUGIN_ID, Messages.getString( "NAModelSimulation.4" ) ) ); //$NON-NLS-1$
        return collector.asMultiStatus( Messages.getString( "NAModelSimulation.5" ) ); //$NON-NLS-1$
      }

      /* Processing. */
      final MultiStatus processStatus = process( naExe, monitor );
      collector.add( processStatus );
      if( monitor.isCanceled() )
      {
        collector.add( new Status( IStatus.CANCEL, ModelNA.PLUGIN_ID, Messages.getString( "NAModelSimulation.6" ) ) ); //$NON-NLS-1$
        return collector.asMultiStatus( Messages.getString( "NAModelSimulation.7" ) ); //$NON-NLS-1$
      }

      /* Post processing. */
      final IStatus postProcessStatus = postProcess( m_data, processStatus, monitor );
      if( !postProcessStatus.isOK() )
        collector.add( postProcessStatus );

      return collector.asMultiStatus( Messages.getString( "NAModelSimulation.8" ) ); //$NON-NLS-1$
    }
    catch( final Exception ex )
    {
      // REMARK: needed, else this exception is probably lost, as the status serialization cannot serialize legacy exceptions
      ex.printStackTrace();

      collector.add( new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, ex.getLocalizedMessage(), ex ) );
      return collector.asMultiStatus( Messages.getString( "NAModelSimulation.9" ) ); //$NON-NLS-1$
    }
  }

  private void validateVersion( final Version calcCoreVersion, final IStatusCollector log )
  {
    if( calcCoreVersion == null )
    {
      final String coreExample = String.format( EXECUTABLES_FILE_TEMPLATE, Messages.getString( "NAModelSimulation.13" ) ); //$NON-NLS-1$
      final String message = String.format( Messages.getString( "NAModelSimulation.14" ), coreExample ); //$NON-NLS-1$
      log.add( IStatus.WARNING, message );
    }
  }

  private File determineCalculationCore( final IStatusCollector log ) throws SimulationException
  {
    try
    {
      final String exeVersion = m_data.getMetaControl().getExeVersion();

      final File naExe = CalcCoreUtils.findExecutable( exeVersion, EXECUTABLES_FILE_TEMPLATE, EXECUTABLES_FILE_PATTERN, CalcCoreUtils.COMPATIBILITY_MODE.NA );
      if( naExe == null )
      {
        final String message = Messages.getString( "KalypsoNaProcessor.0" ); //$NON-NLS-1$
        throw new SimulationException( message );
      }

      // REMARK: file existance has been checked inside the CalcCoreUtils
      return naExe;
    }
    catch( final CoreException e )
    {
      final IStatus status = e.getStatus();
      log.add( status );

      final String msg = status.getMessage();
      throw new SimulationException( msg, e );
    }
  }

  public IStatus rerunForOptimization( final NAOptimize optimize, final ISimulationMonitor monitor ) throws SimulationException, IOException, NAPreprocessorException, NaPostProcessingException
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    // FIXME: clear old result file (we_nat_out etc.)
    // TODO: handle status
    final IStatus preProcessStatus = m_preprocessor.processCallibrationFiles( optimize, monitor );
    if( monitor.isCanceled() )
      return Status.CANCEL_STATUS;
    log.add( preProcessStatus );

    m_processor.run( monitor );
    if( monitor.isCanceled() )
      return Status.CANCEL_STATUS;

    // HACK: move error gml processing into processor
    final MultiStatus processStatus = new MultiStatus( ModelNA.PLUGIN_ID, 0, "PsotProcessing", null ); //$NON-NLS-1$

    final IStatus postProcessStatus = postProcess( m_data, processStatus, monitor );

    log.add( processStatus );
    log.add( postProcessStatus );

    return log.asMultiStatus( "Optimize run" );
  }

  private IStatus preProcess( final INaSimulationData simulationData, final Version calcCoreVersion, final ISimulationMonitor monitor ) throws NAPreprocessorException
  {
    /* Pre processing. */
    m_preprocessor = new NAModelPreprocessor( m_simDirs.asciiDirs, simulationData, calcCoreVersion );

    final IStatus status = m_preprocessor.process( monitor );

    final File idMapFile = new File( m_simDirs.simulationDir, "IdMap.txt" ); //$NON-NLS-1$
    m_preprocessor.getIdManager().dump( idMapFile );

    return status;
  }

  private MultiStatus process( final File naExe, final ISimulationMonitor monitor )
  {
    /* The status collector. */
    final IStatusCollector collector = new StatusCollectorWithTime( ModelNA.PLUGIN_ID );

    try
    {
      /* Processing. */
      m_processor = new KalypsoNaProcessor( m_simDirs.asciiDirs, naExe );
      m_processor.prepare();
      m_processor.run( monitor );
    }
    catch( final SimulationException e )
    {
      collector.add( IStatus.ERROR, e.getLocalizedMessage(), e );
    }

    final String message = Messages.getString( "NAModelSimulation.1", naExe.getName() ); //$NON-NLS-1$
    return collector.asMultiStatus( message );
  }

  private IStatus postProcess( final INaSimulationData simulationData, final MultiStatus processStatus, final ISimulationMonitor monitor ) throws NaPostProcessingException
  {
    /* Monitor. */
    final String messageStartPostprocess = Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.28" ); //$NON-NLS-1$
    monitor.setMessage( messageStartPostprocess );

    /* Post processing. */
    final GMLWorkspace modelWorkspace = simulationData.getModelWorkspace();
    final NAModellControl naControl = simulationData.getNaControl();

    final ICatchmentInfos catchmentData = m_preprocessor.getCatchmentData();
    final IDManager idManager = m_preprocessor.getIdManager();

    final NaPostProcessor postProcessor = new NaPostProcessor( idManager, modelWorkspace, naControl, catchmentData, processStatus );
    return postProcessor.process( m_simDirs.asciiDirs, m_simDirs );
  }

  public NaSimulationDirs getSimulationDirs( )
  {
    return m_simDirs;
  }

  public INaSimulationData getSimulationData( )
  {
    return m_data;
  }

  private Version findCalcCoreVersion( final String filename )
  {
    final Pattern pattern = Pattern.compile( NAModelSimulation.EXECUTABLES_FILE_PATTERN );
    final Matcher matcher = pattern.matcher( filename );

    if( !matcher.matches() )
      return null;

    final String versionString = matcher.group( 1 );

    try
    {
      return new Version( versionString );
    }
    catch( final IllegalArgumentException e )
    {
      System.out.format( "Failed to determine calc core version from filename: %s%n", filename ); //$NON-NLS-1$
      return null;
    }
  }
}