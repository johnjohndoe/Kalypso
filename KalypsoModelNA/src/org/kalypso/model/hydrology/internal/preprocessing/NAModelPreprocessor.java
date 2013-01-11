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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.kalypso.commons.java.net.UrlUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollectorWithTime;
import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.binding.HydrotopeCollection;
import org.kalypso.model.hydrology.binding.NAOptimize;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.control.NAModellControl;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.NaAsciiDirs;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.CatchmentInfo;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.NaCatchmentData;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.ParameterHash;
import org.kalypso.model.hydrology.internal.preprocessing.writer.TimeseriesFileManager;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.SimulationException;

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

  private TimeseriesFileManager m_tsFileManager;

  private NaCatchmentData m_catchmentData;

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

  public IStatus process( final ISimulationMonitor monitor ) throws NAPreprocessorException, SimulationException
  {
    try
    {
      return doProcess( monitor );
    }
    catch( final NAPreprocessorException e )
    {
      throw e;
    }
    catch( final SimulationException e )
    {
      throw e;
    }
    catch( final Exception e )
    {
      // Handle only unexpected exceptions here. Everything else should be handling deeper down!
      e.printStackTrace();
      throw new NAPreprocessorException( Messages.getString( "NAModelPreprocessor.0" ), e ); //$NON-NLS-1$
    }
  }

  private IStatus doProcess( final ISimulationMonitor monitor ) throws SimulationException, NAPreprocessorException, IOException
  {
    final IStatusCollector log = new StatusCollectorWithTime( ModelNA.PLUGIN_ID );

    final NAModellControl naControl = m_simulationData.getNaControl();
    final NAOptimize naOptimize = m_simulationData.getNaOptimize();
    final NAControl metaControl = m_simulationData.getMetaControl();
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

    monitor.setMessage( Messages.getString( "NAModelPreprocessor.1" ) ); //$NON-NLS-1$

    /* build catchments */
    m_simulationData.initLanduseHash( m_logger );

    /* first, dissolve hydrotopes */
    final HydrotopeCollection hydrotopes = m_simulationData.getHydrotopCollection();
    final ParameterHash landuseHash = m_simulationData.getLanduseHash();
    final NaCatchmentData catchmentData = new NaCatchmentData( landuseHash );
    final IStatus status = catchmentData.addHydrotopes( naModel, hydrotopes, true );
    if( !status.isOK() )
      log.add( status );

    final NaModelTweaker naModelTweaker = new NaModelTweaker( naModel, rootNode );
    naModelTweaker.tweakModel();
    checkCancel( monitor );

    monitor.setMessage( Messages.getString( "NAModelPreprocessor.2" ) ); //$NON-NLS-1$
    final NAControlConverter naControlConverter = new NAControlConverter( metaControl, m_asciiDirs.startDir );
    naControlConverter.writeFalstart();
    naControlConverter.writeStartFile( naControl, rootNode, naModel, m_idManager );
    checkCancel( monitor );

    // write net and so on....
    monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.23" ) ); //$NON-NLS-1$
    final NAModellConverter naModellConverter = new NAModellConverter( m_idManager, m_simulationData, m_asciiDirs, m_logger );

    initNetData( rootNode, catchmentData );

    naModellConverter.writeUncalibratedFiles( m_relevantElements, m_tsFileManager, m_catchmentData );
    log.add( naModellConverter.getStatus() );

    final NAOptimize optimizeConfig = m_simulationData.getNaOptimize();
    processCallibrationFiles( optimizeConfig, monitor );

    checkCancel( monitor );

    return log.asMultiStatus( Messages.getString( "NAModelSimulation.10" ) ); //$NON-NLS-1$
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
      throw new SimulationException( Messages.getString( "NAModelPreprocessor.5" ), e ); //$NON-NLS-1$
    }
  }

  public void processCallibrationFiles( final NAOptimize optimize, final ISimulationMonitor monitor ) throws IOException, NAPreprocessorException
  {
    monitor.setMessage( Messages.getString( "NAModelPreprocessor.6" ) ); //$NON-NLS-1$

    final NAModellConverter naModellConverter = new NAModellConverter( m_idManager, m_simulationData, m_asciiDirs, m_logger );

    final CalibrationConfig config = new CalibrationConfig( optimize );
    config.applyCalibrationFactors();

    naModellConverter.writeCalibratedFiles( m_relevantElements, m_tsFileManager );
  }

  private void initNetData( final Node rootNode, final NaCatchmentData catchmentData ) throws SimulationException
  {
    final NaModell naModel = m_simulationData.getNaModel();
    final NAControl metaControl = m_simulationData.getMetaControl();

    final NetFileAnalyser nodeManager = new NetFileAnalyser( rootNode, m_logger, naModel, m_idManager );
    m_relevantElements = nodeManager.analyseNet();

    /* restrict catchment data to relevant elements */
    m_catchmentData = new NaCatchmentData( m_simulationData.getLanduseHash() );

    final Catchment[] relevantCatchments = m_relevantElements.getCatchmentsSorted( m_idManager );
    for( final Catchment relevantCatchment : relevantCatchments )
    {
      final CatchmentInfo relevantInfo = catchmentData.getInfo( relevantCatchment );
      m_catchmentData.addInfo( relevantInfo );
    }

    final boolean usePrecipitationForm = metaControl.isUsePrecipitationForm();
    m_tsFileManager = new TimeseriesFileManager( m_idManager, usePrecipitationForm );
  }

  private void checkCancel( final ISimulationMonitor monitor )
  {
    if( monitor.isCanceled() )
      throw new OperationCanceledException();
  }

  public NaCatchmentData getCatchmentData( )
  {
    return m_catchmentData;
  }
}