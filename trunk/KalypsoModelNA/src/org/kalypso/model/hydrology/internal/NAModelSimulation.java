/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.io.FileUtils;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.manager.IDManager;
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

  private final NAConfiguration m_conf;

  private final IDManager m_idManager;

  private final NaSimulationDirs m_simDirs;

  private NAModelPreprocessor m_preprocessor;

  private final INaSimulationData m_simulationData;

  public NAModelSimulation( final NaSimulationDirs simDirs, final INaSimulationData data, final Logger logger )
  {
    m_simDirs = simDirs;

    m_simulationData = data;
    m_logger = logger;

    m_logger.log( Level.INFO, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.13", m_startDateText ) ); //$NON-NLS-1$ 

    m_conf = new NAConfiguration( m_simDirs.asciiDir );
    m_conf.setSimulationData( m_simulationData );
    final URL context = m_simulationData.getModelWorkspace().getContext();
    m_conf.setZMLContext( context );

    m_idManager = m_conf.getIdManager();
  }

  public boolean runSimulation( final ISimulationMonitor monitor ) throws Exception
  {
    preprocess( m_simulationData, monitor );
    if( monitor.isCanceled() )
      return false;

    process( monitor, m_simulationData );

    if( monitor.isCanceled() )
      return false;

    return postProcess( m_simulationData, monitor );
  }

  public boolean rerunForOptimization( final NAOptimize optimize, final ISimulationMonitor monitor ) throws Exception
  {
    m_preprocessor.processCallibrationFiles( optimize );
    if( monitor.isCanceled() )
      return false;

    process( monitor, m_simulationData );

    if( monitor.isCanceled() )
      return false;

    return postProcess( m_simulationData, monitor );
  }


  private void preprocess( final INaSimulationData simulationData, final ISimulationMonitor monitor ) throws SimulationException
  {
    try
    {
      m_preprocessor = new NAModelPreprocessor( m_conf, m_simDirs.asciiDirs, m_idManager, simulationData, m_logger );
      m_preprocessor.process( monitor );

      final File idMapFile = new File( m_simDirs.simulationDir, "IdMap.txt" ); //$NON-NLS-1$
      m_idManager.dump( idMapFile );
    }
    catch( final NAPreprocessorException e )
    {
      final String msg = String.format( "Failed to convert data in Kalypso-NA.exe format files: %s", e.getLocalizedMessage() );
      m_logger.log( Level.SEVERE, msg, e );
      throw new SimulationException( msg );
    }
  }

  public void backupResults( )
  {
    final File resultDirTo = new File( m_simDirs.resultDir, m_startDateText ); //$NON-NLS-1$

    try
    {
      // Copy results to restore the actual results in the dateDir as well... .
      // FIXME: probably this should be done by the ant task instead
      FileUtils.copyDirectory( m_simDirs.currentResultDir, resultDirTo );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      final String msg = String.format( "Failed to backup results to directory: %s (%s)", resultDirTo.getAbsoluteFile(), e.getLocalizedMessage() );
      m_logger.warning( msg );
    }
  }

  private void process( final ISimulationMonitor monitor, final INaSimulationData simulationData ) throws SimulationException
  {
    monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.27" ) ); //$NON-NLS-1$

    final NAControl metaControl = simulationData.getMetaControl();
    final String exeVersion = metaControl.getExeVersion();
    final KalypsoNaProcessor processor = new KalypsoNaProcessor( m_simDirs.asciiDirs, exeVersion );
    processor.run( monitor );
  }

  private boolean postProcess( final INaSimulationData simulationData, final ISimulationMonitor monitor ) throws Exception
  {
    final String messageStartPostprocess = Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.28" ); //$NON-NLS-1$
    monitor.setMessage( messageStartPostprocess ); 
    m_logger.log( Level.FINEST, messageStartPostprocess );

    final GMLWorkspace modelWorkspace = simulationData.getModelWorkspace();
    final NAModellControl naControl = simulationData.getNaControl();
    final NAOptimize naOptimize = simulationData.getNaOptimize();

    final HydroHash hydroHash = m_preprocessor.getHydroHash();

    final NaPostProcessor postProcessor = new NaPostProcessor( m_conf, m_logger, modelWorkspace, naControl, naOptimize, hydroHash );
    postProcessor.process( m_simDirs.asciiDirs, m_simDirs );
    return postProcessor.isSucceeded();
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
