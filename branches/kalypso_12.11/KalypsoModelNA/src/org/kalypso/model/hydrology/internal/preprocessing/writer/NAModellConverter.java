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
  private final INaPreparedData m_data;

  private final NaAsciiDirs m_asciiDirs;

  public NAModellConverter( final INaPreparedData preparedData, final NaAsciiDirs asciiDirs )
  {
    m_data = preparedData;
    m_asciiDirs = asciiDirs;
  }

  public IStatus writeUncalibratedFiles( ) throws IOException, NAPreprocessorException
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    final NaModell naModel = m_data.getModel();
    final NAControl metaControl = m_data.getMetaControl();
    final GMLWorkspace synthNWorkspace = m_data.getSynthNWorkspace();
    final NAOptimize naOptimize = m_data.getNaOptimize();
    final IDManager idManager = m_data.getIdManager();
    final RelevantNetElements relevantElements = m_data.getRelevantElements();
    final TimeseriesFileManager tsFileManager = m_data.getTimeseriesManager();
    final Parameter parameter = m_data.getParameter();
    final ParameterHash landuseHash = m_data.getLanduseHash();
    final NaCatchmentData catchmentData = m_data.getCatchmentData();
    final InitialValues initialValues = m_data.getInitialValues();
    final Version calcCoreVersion = m_data.getCalcCoreVersion();

    final URL zmlContext = naModel.getWorkspace().getContext();

    final NetElement[] channels = relevantElements.getChannelsSorted( idManager );
    final Catchment[] catchments = relevantElements.getCatchmentsSorted( idManager );

    final NetFileWriter netWriter = new NetFileWriter( m_asciiDirs, relevantElements, idManager, zmlContext, metaControl );
    netWriter.write( m_asciiDirs.netFile, log );

    final TsFileWriter tsWriter = new TsFileWriter( synthNWorkspace, metaControl, naOptimize, channels, zmlContext, tsFileManager );
    tsWriter.write( m_asciiDirs.klimaDatDir );

    // HACK: for performance optimization: if the zft file already exists, we assume it is ok and just return
    if( !m_asciiDirs.zftFile.exists() )
    {
      final ZftWriter zftWriter = new ZftWriter( idManager, catchments );
      zftWriter.write( m_asciiDirs.zftFile, log );
    }

    final BodenartWriter bodenartManager = new BodenartWriter( parameter );
    bodenartManager.write( m_asciiDirs.bodenartFile, log );

    final BodentypWriter bodentypManager = new BodentypWriter( parameter, calcCoreVersion );
    bodentypManager.write( m_asciiDirs.bodentypFile, log );

    final SnowtypWriter schneeManager = new SnowtypWriter( parameter );
    schneeManager.write( m_asciiDirs.schneeFile, log );

    final HRBFileWriter hrbFileWriter = new HRBFileWriter( channels, idManager, m_asciiDirs.klimaDatDir );
    hrbFileWriter.write( m_asciiDirs.hrbFile, log );

    final NutzungWriter nutzungManager = new NutzungWriter( parameter, m_asciiDirs.hydroTopDir );
    nutzungManager.writeFile( landuseHash );

    final HydrotopeWriter hydrotopManager = new HydrotopeWriter( idManager, catchmentData );
    hydrotopManager.write( m_asciiDirs.hydrotopFile, log );

    final LzsimWriter lzsimWriter = new LzsimWriter( idManager, catchmentData, initialValues );
    lzsimWriter.writeLzsimFiles( m_asciiDirs.lzsimDir );

    return log.asMultiStatusOrOK( "Writing basic files" ); //$NON-NLS-1$
  }

  public IStatus writeCalibratedFiles( ) throws IOException, NAPreprocessorException
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    final NAControl naControl = m_data.getMetaControl();
    final IDManager idManager = m_data.getIdManager();
    final RelevantNetElements relevantElements = m_data.getRelevantElements();
    final TimeseriesFileManager tsFileManager = m_data.getTimeseriesManager();

    final Entry<NetElement, Integer>[] rootChannels = relevantElements.getRootChannels();
    final NetElement[] channels = relevantElements.getChannelsSorted( idManager );
    final Catchment[] catchments = relevantElements.getCatchmentsSorted( idManager );

    /* Write files that are changed by calibration factors */
    final GerWriter gerWriter = new GerWriter( idManager, rootChannels, channels );
    gerWriter.write( m_asciiDirs.channelFile, log );

    final GebWriter catchmentManager = new GebWriter( catchments, naControl, tsFileManager, idManager );
    catchmentManager.write( m_asciiDirs.catchmentFile, log );

    return log.asMultiStatusOrOK( Messages.getString("NAModellConverter_0") ); //$NON-NLS-1$
  }
}