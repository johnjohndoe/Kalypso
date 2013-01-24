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
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollectorWithTime;
import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.NAModellControl;
import org.kalypso.model.hydrology.binding.NAOptimize;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.postprocessing.NaPostProcessor;
import org.kalypso.model.hydrology.internal.preprocessing.NAModelPreprocessor;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.HydroHash;
import org.kalypso.model.hydrology.internal.processing.KalypsoNaProcessor;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Gernot Belger
 */
public class NAModelSimulation
{
  private final DateFormat START_DATE_FORMAT = new SimpleDateFormat( "yyyy-MM-dd(HH-mm-ss)" ); //$NON-NLS-1$

  private final String m_startDateText = START_DATE_FORMAT.format( new Date() );

  private final Logger m_logger;

  private final NaSimulationDirs m_simDirs;

  private NAModelPreprocessor m_preprocessor;

  private final INaSimulationData m_simulationData;

  private KalypsoNaProcessor m_processor;

  public NAModelSimulation( final NaSimulationDirs simDirs, final INaSimulationData data, final Logger logger )
  {
    m_simDirs = simDirs;

    m_simulationData = data;
    m_logger = logger;

    m_logger.log( Level.INFO, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.13", m_startDateText ) ); //$NON-NLS-1$ 
  }

  public IStatus runSimulation( final ISimulationMonitor monitor ) throws Exception
  {
    /* The status collector. */
    final IStatusCollector collector = new StatusCollectorWithTime( ModelNA.PLUGIN_ID );

    try
    {
      monitor.setMessage( "Simulation wird gestartet..." );

      preprocess( m_simulationData, monitor );

      /* Pre processing. */
      final IStatus preProcessStatus = preprocess( m_simulationData, monitor );
      collector.add( preProcessStatus );
      if( monitor.isCanceled() )
      {
        collector.add( new Status( IStatus.CANCEL, ModelNA.PLUGIN_ID, "Simulation abgebrochen..." ) );
        return collector.asMultiStatus( "Simulation abgebrochen..." );
      }

      /* Processing. */
      final IStatus processStatus = process( m_simulationData, monitor );
      collector.add( processStatus );
      if( monitor.isCanceled() )
      {
        collector.add( new Status( IStatus.CANCEL, ModelNA.PLUGIN_ID, "Simulation abgebrochen..." ) );
        return collector.asMultiStatus( "Simulation abgebrochen..." );
      }

      /* Post processing. */
      final IStatus postProcessStatus = postProcess( m_simulationData, monitor );
      collector.add( postProcessStatus );

      return collector.asMultiStatus( "Simulation abgeschlossen" );
    }
    catch( final Exception ex )
    {
      collector.add( new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, ex.getLocalizedMessage(), ex ) );
      return collector.asMultiStatus( "Simulation fehlgeschlagen" );
    }
  }

  public IStatus rerunForOptimization( final NAOptimize optimize, final ISimulationMonitor monitor ) throws Exception
  {
    /* The status collector. */
    final IStatusCollector collector = new StatusCollectorWithTime( ModelNA.PLUGIN_ID );

    // FIXME: clear old result file (we_nat_out etc.)
    m_preprocessor.processCallibrationFiles( optimize, monitor );
    if( monitor.isCanceled() )
      return Status.CANCEL_STATUS;
    // FIXME: get status from preprocessor method
    collector.add( IStatus.OK, "Vorverarbeitung" );

    m_processor.run( monitor );
    // FIXME: get status from preprocessor method
    collector.add( IStatus.OK, "Verarbeitung" );

    if( monitor.isCanceled() )
      return Status.CANCEL_STATUS;

    final IStatus postStatus = postProcess( m_simulationData, monitor );
    collector.add( postStatus );

    return collector.asMultiStatus( "Simulation" );
  }

  private IStatus preprocess( final INaSimulationData simulationData, final ISimulationMonitor monitor ) throws SimulationException
  {
    try
    {
      m_preprocessor = new NAModelPreprocessor( m_simDirs.asciiDirs, simulationData, m_logger );
      m_preprocessor.process( monitor );

      final File idMapFile = new File( m_simDirs.simulationDir, "IdMap.txt" ); //$NON-NLS-1$
      m_preprocessor.getIdManager().dump( idMapFile );

      return new Status( IStatus.OK, ModelNA.PLUGIN_ID, "Vorverarbeitung" );
    }
    catch( final NAPreprocessorException e )
    {
      final String msg = String.format( "Failed to convert data in Kalypso-NA.exe format files: %s", e.getLocalizedMessage() );
      m_logger.log( Level.SEVERE, msg, e );
      throw new SimulationException( msg, e );
    }
  }

  private IStatus process( final INaSimulationData simulationData, final ISimulationMonitor monitor )
  {
    /* The status collector. */
    final IStatusCollector collector = new StatusCollectorWithTime( ModelNA.PLUGIN_ID );

    try
    {
      /* Processing. */
      final NAControl metaControl = simulationData.getMetaControl();
      final String exeVersion = metaControl.getExeVersion();
      m_processor = new KalypsoNaProcessor( m_simDirs.asciiDirs, exeVersion );
      m_processor.prepare();
      m_processor.run( monitor );
    }
    catch( final SimulationException e )
    {
      collector.add( IStatus.ERROR, e.getLocalizedMessage(), e );
    }

    return collector.asMultiStatus( "Verarbeitung" );
  }

  private IStatus postProcess( final INaSimulationData simulationData, final ISimulationMonitor monitor ) throws Exception
  {
    final String messageStartPostprocess = Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.28" ); //$NON-NLS-1$
    monitor.setMessage( messageStartPostprocess );
    m_logger.log( Level.FINEST, messageStartPostprocess );

    final GMLWorkspace modelWorkspace = simulationData.getModelWorkspace();
    final NAModellControl naControl = simulationData.getNaControl();

    final HydroHash hydroHash = m_preprocessor.getHydroHash();
    final IDManager idManager = m_preprocessor.getIdManager();

    final NaPostProcessor postProcessor = new NaPostProcessor( idManager, m_logger, modelWorkspace, naControl, hydroHash );
    postProcessor.process( m_simDirs.asciiDirs, m_simDirs );

    final boolean succeeded = postProcessor.isSucceeded();

    if( succeeded )
      return new Status( IStatus.OK, ModelNA.PLUGIN_ID, "Nachverarbeitung" );
    else
      return new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, "Nachverarbeitung fehlgeschlagen" );
  }

  public INaSimulationData getSimulationData( )
  {
    return m_simulationData;
  }

  public NaSimulationDirs getSimulationDirs( )
  {
    return m_simDirs;
  }
}