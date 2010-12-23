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
import java.net.MalformedURLException;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.io.FileUtils;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NaSimulationData;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.convert.namodel.optimize.NAOptimizingJob;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.NAModellControl;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.postprocessing.NaPostProcessor;
import org.kalypso.model.hydrology.internal.preprocessing.NAModelPreprocessor;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.HydroHash;
import org.kalypso.model.hydrology.internal.processing.KalypsoNaProcessor;
import org.kalypso.simulation.core.ISimulationDataProvider;
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

  private final ISimulationDataProvider m_inputProvider;

  private final Logger m_logger;

  private final NAConfiguration m_conf;

  private final IDManager m_idManager;

  private final NaSimulationDirs m_simDirs;

  public NAModelSimulation( final NaSimulationDirs simDirs, final ISimulationDataProvider inputProvider, final Logger logger ) throws SimulationException
  {
    m_simDirs = simDirs;
    m_inputProvider = inputProvider;
    m_logger = logger;

    m_logger.log( Level.INFO, Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.13", m_startDateText ) ); //$NON-NLS-1$ 

    m_conf = new NAConfiguration( simDirs.asciiDir );
    m_conf.setZMLContext( (URL) inputProvider.getInputForID( NaModelConstants.IN_META_ID ) );

    m_idManager = m_conf.getIdManager();
  }

  public boolean runSimulation( final ISimulationMonitor monitor ) throws Exception
  {
    final NaSimulationData simulationData = loadData();
    m_conf.setSimulationData( simulationData );

    final HydroHash hydroHash = preprocess( simulationData, monitor );
    if( monitor.isCanceled() )
      return false;

    process( monitor, simulationData );

    if( monitor.isCanceled() )
      return false;

    return postProcess( simulationData, hydroHash, monitor );
  }

  private NaSimulationData loadData( ) throws Exception
  {
    final URL modelUrl = (URL) m_inputProvider.getInputForID( NaModelConstants.IN_MODELL_ID );
    final URL controlURL = (URL) m_inputProvider.getInputForID( NaModelConstants.IN_CONTROL_ID );
    final URL metaUrl = (URL) m_inputProvider.getInputForID( NaModelConstants.IN_META_ID );
    final URL parameterUrl = (URL) m_inputProvider.getInputForID( NaModelConstants.IN_PARAMETER_ID );
    final URL hydrotopUrl = (URL) m_inputProvider.getInputForID( NaModelConstants.IN_HYDROTOP_ID );
    final URL syntNUrl = (URL) m_inputProvider.getInputForID( NaModelConstants.IN_RAINFALL_ID );
    final URL lzsimUrl = getStartConditionFile();

    final URL sudsUrl = getInputOrNull( NaModelConstants.IN_SUDS_ID );

    return new NaSimulationData( modelUrl, controlURL, metaUrl, parameterUrl, hydrotopUrl, sudsUrl, syntNUrl, lzsimUrl );
  }

  private URL getInputOrNull( final String id ) throws SimulationException
  {
    if( !m_inputProvider.hasID( id ) )
      return null;

    return (URL) m_inputProvider.getInputForID( id );
  }

  private HydroHash preprocess( final NaSimulationData simulationData, final ISimulationMonitor monitor ) throws SimulationException
  {
    try
    {
      final NAModelPreprocessor preprocessor = new NAModelPreprocessor( m_conf, m_simDirs.asciiDirs, m_idManager, simulationData, m_logger );
      preprocessor.setPreprocessedFilesDir( getPreprocessFilesDir() );
      preprocessor.process( monitor );

      final File idMapFile = new File( m_simDirs.simulationDir, "IdMap.txt" ); //$NON-NLS-1$
      m_idManager.dump( idMapFile );

      return preprocessor.getHydroHash();
    }
    catch( final NAPreprocessorException e )
    {
      final String msg = String.format( "Failed to convert data in Kalypso-NA.exe format files: %s", e.getLocalizedMessage() );
      m_logger.log( Level.SEVERE, msg, e );
      throw new SimulationException( msg );
    }
  }

  private URL getStartConditionFile( ) throws SimulationException
  {
    if( !m_inputProvider.hasID( NaModelConstants.LZSIM_IN_ID ) )
      return null;

    final URL iniValuesFolderURL = (URL) m_inputProvider.getInputForID( NaModelConstants.LZSIM_IN_ID );
    try
    {
      // TODO: crude way to create the new URL, necessary as probably we do not have a '/' at the end of the path
      return new URL( iniValuesFolderURL.toExternalForm() + "/lzsim.gml" ); //$NON-NLS-1$
    }
    catch( final MalformedURLException e )
    {
      throw new SimulationException( "Failed to read start condition file", e );
    }
  }


  private File getPreprocessFilesDir( ) throws SimulationException
  {
    if( !m_inputProvider.hasID( NAOptimizingJob.IN_BestOptimizedRunDir_ID ) )
      return null;

    final URL url = (URL) m_inputProvider.getInputForID( NAOptimizingJob.IN_BestOptimizedRunDir_ID );
    return FileUtils.toFile( url );
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

  private void process( final ISimulationMonitor monitor, final NaSimulationData simulationData ) throws SimulationException
  {
    monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.27" ) ); //$NON-NLS-1$

    final NAControl metaControl = simulationData.getMetaControl();
    final String exeVersion = metaControl.getExeVersion();
    final KalypsoNaProcessor processor = new KalypsoNaProcessor( m_simDirs.asciiDirs, exeVersion );
    processor.run( monitor );
  }

  private boolean postProcess( final NaSimulationData simulationData, final HydroHash hydroHash, final ISimulationMonitor monitor ) throws Exception
  {
    final String messageStartPostprocess = Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.28" ); //$NON-NLS-1$
    monitor.setMessage( messageStartPostprocess ); 
    m_logger.log( Level.FINEST, messageStartPostprocess );

    final GMLWorkspace modelWorkspace = simulationData.getModelWorkspace();
    final NAModellControl naControl = simulationData.getNaControl();

    final NaPostProcessor postProcessor = new NaPostProcessor( m_conf, m_logger, modelWorkspace, naControl, hydroHash );
    postProcessor.process( m_simDirs.asciiDirs, m_simDirs );
    return postProcessor.isSucceeded();
  }
}
