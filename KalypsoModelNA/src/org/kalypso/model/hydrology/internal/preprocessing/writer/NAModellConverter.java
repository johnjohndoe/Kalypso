/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.io.IOException;
import java.net.URL;
import java.util.Map.Entry;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.NAOptimize;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.initialValues.InitialValues;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.NaAsciiDirs;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.NaCatchmentData;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.ParameterHash;
import org.kalypso.model.hydrology.internal.preprocessing.preparation.INaPreparedData;
import org.kalypso.model.hydrology.internal.preprocessing.preparation.NetElement;
import org.kalypso.model.hydrology.internal.preprocessing.preparation.RelevantNetElements;
import org.kalypso.model.hydrology.internal.preprocessing.preparation.TimeseriesFileManager;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.osgi.framework.Version;

/**
 * Import kalypso rainfall runoff models converts between custom ascii format and gml format. Export to ascii can be
 * generated from a gml file or from a gml workspace.
 * 
 * @author doemming
 */
class NAModellConverter
{
  private final IStatusCollector m_log = new StatusCollector( ModelNA.PLUGIN_ID );

  private final INaPreparedData m_data;

  private final NaAsciiDirs m_asciiDirs;

  public NAModellConverter( final INaPreparedData preparedData, final NaAsciiDirs asciiDirs )
  {
    m_data = preparedData;
    m_asciiDirs = asciiDirs;
  }

  public void writeUncalibratedFiles( ) throws IOException, NAPreprocessorException, SimulationException
  {
    final NaModell naModel = m_data.getModel();
    final NAControl metaControl = m_data.getMetaControl();
    final GMLWorkspace synthNWorkspace = m_data.getSynthNWorkspace();
    final NAOptimize naOptimize = m_data.getNaOptimize();
    final IDManager idManager = m_data.getIdManager();
    final RelevantNetElements relevantElements = m_data.getRelevantElements();
    final Logger logger = m_data.getLogger();
    final TimeseriesFileManager tsFileManager = m_data.getTimeseriesManager();
    final Parameter parameter = m_data.getParameter();
    final ParameterHash landuseHash = m_data.getLanduseHash();
    final NaCatchmentData catchmentData = m_data.getCatchmentData();
    final InitialValues initialValues = m_data.getInitialValues();
    final Version calcCoreVersion = m_data.getCalcCoreVersion();

    final URL zmlContext = naModel.getWorkspace().getContext();

    final NetElement[] channels = relevantElements.getChannelsSorted( idManager );
    final Catchment[] catchments = relevantElements.getCatchmentsSorted( idManager );

    final NetFileWriter netWriter = new NetFileWriter( m_asciiDirs, relevantElements, idManager, zmlContext, metaControl, logger );
    netWriter.write( m_asciiDirs.netFile );
    m_log.add( netWriter.getStatus() );

    final TsFileWriter tsWriter = new TsFileWriter( synthNWorkspace, metaControl, naOptimize, channels, zmlContext, tsFileManager, logger );
    tsWriter.write( m_asciiDirs.klimaDatDir );

    // HACK: for performance optimization: if the zft file already exists, we assume it is ok and just return
    if( !m_asciiDirs.zftFile.exists() )
    {
      final ZftWriter zftWriter = new ZftWriter( idManager, logger, catchments );
      zftWriter.write( m_asciiDirs.zftFile );
    }

    final BodenartWriter bodenartManager = new BodenartWriter( parameter, logger );
    bodenartManager.write( m_asciiDirs.bodenartFile );

    final BodentypWriter bodentypManager = new BodentypWriter( parameter, calcCoreVersion, logger );
    bodentypManager.write( m_asciiDirs.bodentypFile );

    final SnowtypWriter schneeManager = new SnowtypWriter( parameter, logger );
    schneeManager.write( m_asciiDirs.schneeFile );

    final HRBFileWriter hrbFileWriter = new HRBFileWriter( channels, idManager, m_asciiDirs.klimaDatDir, logger );
    hrbFileWriter.write( m_asciiDirs.hrbFile );

    final NutzungWriter nutzungManager = new NutzungWriter( parameter, m_asciiDirs.hydroTopDir );
    nutzungManager.writeFile( landuseHash );

    final HydrotopeWriter hydrotopManager = new HydrotopeWriter( idManager, catchmentData, logger );
    hydrotopManager.write( m_asciiDirs.hydrotopFile );

    final LzsimWriter lzsimWriter = new LzsimWriter( idManager, catchmentData, initialValues, metaControl, logger );
    lzsimWriter.writeLzsimFiles( m_asciiDirs.lzsimDir );
  }

  public void writeCalibratedFiles( ) throws IOException, NAPreprocessorException
  {
    final NAControl naControl = m_data.getMetaControl();
    final IDManager idManager = m_data.getIdManager();
    final RelevantNetElements relevantElements = m_data.getRelevantElements();
    final Logger logger = m_data.getLogger();
    final TimeseriesFileManager tsFileManager = m_data.getTimeseriesManager();

    final Entry<NetElement, Integer>[] rootChannels = relevantElements.getRootChannels();
    final NetElement[] channels = relevantElements.getChannelsSorted( idManager );
    final Catchment[] catchments = relevantElements.getCatchmentsSorted( idManager );

    /* Write files that are changed by calibration factors */
    final GerWriter gerWriter = new GerWriter( idManager, rootChannels, channels, logger );
    gerWriter.write( m_asciiDirs.channelFile );

    final GebWriter catchmentManager = new GebWriter( logger, catchments, naControl, tsFileManager, idManager );
    catchmentManager.write( m_asciiDirs.catchmentFile );
  }

  public IStatus getStatus( )
  {
    return m_log.asMultiStatusOrOK( Messages.getString( "NAModellConverter_0" ) ); //$NON-NLS-1$
  }
}