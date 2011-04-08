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
package org.kalypso.model.hydrology.internal.preprocessing;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.logging.Logger;

import org.eclipse.core.runtime.OperationCanceledException;
import org.kalypso.commons.java.net.UrlUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.NAHydrotop;
import org.kalypso.model.hydrology.binding.NAModellControl;
import org.kalypso.model.hydrology.binding.NAOptimize;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.Node;
import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.NaAsciiDirs;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.HydroHash;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.LanduseHash;
import org.kalypso.model.hydrology.internal.preprocessing.writer.TimeseriesFileManager;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;

/**
 * Converts KalypsoHydrology gml files to Kalypso-NA ascii files.
 * 
 * @author Gernot Belger
 */
public class NAModelPreprocessor
{
  private RelevantNetElements m_relevantElements;

  private final IDManager m_idManager = new IDManager();

  private final INaSimulationData m_simulationData;

  private final Logger m_logger;

  private final NaAsciiDirs m_asciiDirs;

  private HydroHash m_hydroHash;

  private TimeseriesFileManager m_tsFileManager;

  public NAModelPreprocessor( final NaAsciiDirs asciiDirs, final INaSimulationData simulationData, final Logger logger )
  {
    m_asciiDirs = asciiDirs;
    m_simulationData = simulationData;
    m_logger = logger;
  }

  public IDManager getIdManager( )
  {
    return m_idManager;
  }

  public void process( final ISimulationMonitor monitor ) throws NAPreprocessorException, OperationCanceledException
  {
    try
    {
      doProcess( monitor );
    }
    catch( final NAPreprocessorException e )
    {
      throw e;
    }
    catch( final Exception e )
    {
      // Handle only unexpected exceptions here. Everything else should be handling deeper down!
      e.printStackTrace();
      throw new NAPreprocessorException( "Unexpected error while generating Kalypso-NA ASCII files", e );
    }
  }

  // FIXME: improve error handling! Do not throw generic Exception!
  private void doProcess( final ISimulationMonitor monitor ) throws Exception
  {
    final NAModellControl naControl = m_simulationData.getNaControl();
    final NAOptimize naOptimize = m_simulationData.getNaOptimize();
    final NAControl metaControl = m_simulationData.getMetaControl();
    final GMLWorkspace sudsWorkspace = m_simulationData.getSudsWorkspace();
    final URL preprocesssedASCII = m_simulationData.getPreprocessedASCII();
    final NaModell naModel = m_simulationData.getNaModel();

    final Node rootNode = naOptimize == null ? null : naOptimize.getRootNode();

    monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.15" ) ); //$NON-NLS-1$
    checkCancel( monitor );

    m_asciiDirs.inpDir.mkdirs();
    m_asciiDirs.klimaDatDir.mkdirs();
    m_asciiDirs.hydroTopDir.mkdirs();
    m_asciiDirs.zuflussDir.mkdirs();
    m_asciiDirs.outWeNatDir.mkdirs();

    handlePreprocessedAsciiFiles( preprocesssedASCII, m_asciiDirs.asciiDir );

    checkCancel( monitor );

    monitor.setMessage( "Adding additional virtual channels" );
    final NaModelTweaker naModelTweaker = new NaModelTweaker( naModel, rootNode );
    naModelTweaker.tweakModel();
    checkCancel( monitor );

    monitor.setMessage( "Writing control files for Kalypso-NA" );
    final NAControlConverter naControlConverter = new NAControlConverter( metaControl, m_asciiDirs.startDir );
    naControlConverter.writeFalstart();
    naControlConverter.writeStartFile( naControl, rootNode, naModel, sudsWorkspace, m_idManager );
    checkCancel( monitor );

    // write net and so on....
    monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.23" ) ); //$NON-NLS-1$
    final NAModellConverter naModellConverter = new NAModellConverter( m_idManager, m_simulationData, m_asciiDirs, m_logger );
    initNetData( rootNode );
    naModellConverter.writeUncalibratedFiles( m_relevantElements, m_tsFileManager, m_hydroHash );

    final NAOptimize optimizeConfig = m_simulationData.getNaOptimize();
    processCallibrationFiles( optimizeConfig, monitor );

    checkCancel( monitor );
  }

  private void handlePreprocessedAsciiFiles( final URL preprocesssedASCII, final File asciiDir ) throws SimulationException
  {
    try
    {
      if( preprocesssedASCII == null || !UrlUtilities.checkIsAccessible( preprocesssedASCII ) )
        return;

      ZipUtilities.unzip( preprocesssedASCII, asciiDir );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new SimulationException( "Fehler beim Entzippen der Vorprozessierten ASCII Dateien", e );
    }
  }

  public void processCallibrationFiles( final NAOptimize optimize, final ISimulationMonitor monitor ) throws Exception
  {
    monitor.setMessage( "Wende Kalibrierungsfaktoren an..." );

    final NAModellConverter naModellConverter = new NAModellConverter( m_idManager, m_simulationData, m_asciiDirs, m_logger );

    final CalibrationConfig config = new CalibrationConfig( optimize );
    config.applyCalibrationFactors();

    naModellConverter.writeCalibratedFiles( m_relevantElements, m_tsFileManager );
  }

  private void initNetData( final Node rootNode ) throws SimulationException, Exception
  {
    final NaModell naModel = m_simulationData.getNaModel();
    final NAHydrotop hydrotopeCollection = m_simulationData.getHydrotopCollection();
    final GMLWorkspace parameterWorkspace = m_simulationData.getParameterWorkspace();
    final NAControl metaControl = m_simulationData.getMetaControl();
    final Parameter parameter = (Parameter) parameterWorkspace.getRootFeature();

    final NetFileAnalyser m_nodeManager = new NetFileAnalyser( rootNode, m_logger, naModel, m_idManager );
    m_relevantElements = m_nodeManager.analyseNet();

    if( hydrotopeCollection != null )
    {
      final Catchment[] catchments = m_relevantElements.getCatchmentsSorted( m_idManager );
      initHydroHash( parameter, hydrotopeCollection, catchments );
    }

    final boolean usePrecipitationForm = metaControl.isUsePrecipitationForm();
    m_tsFileManager = new TimeseriesFileManager( m_idManager, usePrecipitationForm );
  }

  private HydroHash initHydroHash( final Parameter parameter, final NAHydrotop hydrotopeCollection, final Catchment[] catchments ) throws GM_Exception, SimulationException
  {
    if( m_hydroHash == null )
    {
      final LanduseHash landuseHash = new LanduseHash( parameter, m_logger );
      m_hydroHash = new HydroHash( landuseHash );
      m_hydroHash.initHydrotopes( hydrotopeCollection, catchments );
    }

    return m_hydroHash;
  }

  private void checkCancel( final ISimulationMonitor monitor )
  {
    if( monitor.isCanceled() )
      throw new OperationCanceledException();
  }

  public HydroHash getHydroHash( )
  {
    return m_hydroHash;
  }
}
