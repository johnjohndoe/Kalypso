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
package org.kalypso.model.hydrology.internal.preprocessing;

import java.io.IOException;
import java.net.URL;
import java.util.Map.Entry;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.binding.NAOptimize;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.initialValues.InitialValues;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.NaAsciiDirs;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.NaCatchmentData;
import org.kalypso.model.hydrology.internal.preprocessing.net.NetElement;
import org.kalypso.model.hydrology.internal.preprocessing.writer.BodenartWriter;
import org.kalypso.model.hydrology.internal.preprocessing.writer.BodentypWriter;
import org.kalypso.model.hydrology.internal.preprocessing.writer.GebWriter;
import org.kalypso.model.hydrology.internal.preprocessing.writer.GerWriter;
import org.kalypso.model.hydrology.internal.preprocessing.writer.HRBFileWriter;
import org.kalypso.model.hydrology.internal.preprocessing.writer.HydrotopeWriter;
import org.kalypso.model.hydrology.internal.preprocessing.writer.LzsimWriter;
import org.kalypso.model.hydrology.internal.preprocessing.writer.NetFileWriter;
import org.kalypso.model.hydrology.internal.preprocessing.writer.NutzungWriter;
import org.kalypso.model.hydrology.internal.preprocessing.writer.SnowtypWriter;
import org.kalypso.model.hydrology.internal.preprocessing.writer.TimeseriesFileManager;
import org.kalypso.model.hydrology.internal.preprocessing.writer.TsFileWriter;
import org.kalypso.model.hydrology.internal.preprocessing.writer.ZftWriter;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Import kalypso rainfall runoff models converts between custom ascii format and gml format. Export to ascii can be
 * generated from a gml file or from a gml workspace.
 * 
 * @author doemming
 */
public class NAModellConverter
{
  private final IStatusCollector m_log = new StatusCollector( ModelNA.PLUGIN_ID );

  private final Logger m_logger;

  private final INaSimulationData m_data;

  private final NaAsciiDirs m_asciiDirs;

  private final IDManager m_idManager;

  public NAModellConverter( final IDManager idManager, final INaSimulationData data, final NaAsciiDirs asciiDirs, final Logger logger )
  {
    m_idManager = idManager;
    m_data = data;
    m_asciiDirs = asciiDirs;
    m_logger = logger;
  }

  public void writeUncalibratedFiles( final RelevantNetElements relevantElements, final TimeseriesFileManager tsFileManager, final NaCatchmentData catchmentData ) throws IOException, NAPreprocessorException, SimulationException
  {
    final NaModell naModel = m_data.getNaModel();
    final NAControl metaControl = m_data.getMetaControl();
    final GMLWorkspace synthNWorkspace = m_data.getSynthNWorkspace();
    final NAOptimize naOptimize = m_data.getNaOptimize();

    final URL zmlContext = naModel.getWorkspace().getContext();

    final NetElement[] channels = relevantElements.getChannelsSorted( m_idManager );
    final Catchment[] catchments = relevantElements.getCatchmentsSorted( m_idManager );

    final NetFileWriter netWriter = new NetFileWriter( m_asciiDirs, relevantElements, m_idManager, zmlContext, metaControl, m_logger );
    netWriter.write( m_asciiDirs.netFile );
    m_log.add( netWriter.getStatus() );

    final TsFileWriter tsWriter = new TsFileWriter( synthNWorkspace, metaControl, naOptimize, channels, zmlContext, tsFileManager, m_logger );
    tsWriter.write( m_asciiDirs.klimaDatDir );

    // HACK: for performance optimization: if the zft file already exists, we assume it is ok and just return
    if( !m_asciiDirs.zftFile.exists() )
    {
      final ZftWriter zftWriter = new ZftWriter( m_idManager, m_logger, catchments );
      zftWriter.write( m_asciiDirs.zftFile );
    }

    final BodenartWriter bodenartManager = new BodenartWriter( m_data, m_logger );
    bodenartManager.write( m_asciiDirs.bodenartFile );

    final BodentypWriter bodentypManager = new BodentypWriter( m_data, m_logger );
    bodentypManager.write( m_asciiDirs.bodentypFile );

    final SnowtypWriter schneeManager = new SnowtypWriter( m_data, m_logger );
    schneeManager.write( m_asciiDirs.schneeFile );

    final HRBFileWriter hrbFileWriter = new HRBFileWriter( channels, m_idManager, m_asciiDirs.klimaDatDir, m_logger );
    hrbFileWriter.write( m_asciiDirs.hrbFile );

    if( catchmentData != null )
    {
      final NutzungWriter nutzungManager = new NutzungWriter( m_data, m_asciiDirs.hydroTopDir );
      nutzungManager.writeFile( m_data.getLanduseHash() );

      final HydrotopeWriter hydrotopManager = new HydrotopeWriter( m_idManager, catchmentData, m_logger );
      hydrotopManager.write( m_asciiDirs.hydrotopFile );

      final InitialValues initialValues = m_data.getInitialValues();
      final LzsimWriter lzsimWriter = new LzsimWriter( m_idManager, catchmentData, initialValues, metaControl, m_logger );
      lzsimWriter.writeLzsimFiles( m_asciiDirs.lzsimDir );
    }
  }

  public void writeCalibratedFiles( final RelevantNetElements relevantElements, final TimeseriesFileManager tsFileManager ) throws IOException, NAPreprocessorException
  {
    final NAControl naControl = m_data.getMetaControl();

    final Entry<NetElement, Integer>[] rootChannels = relevantElements.getRootChannels();
    final NetElement[] channels = relevantElements.getChannelsSorted( m_idManager );
    final Catchment[] catchments = relevantElements.getCatchmentsSorted( m_idManager );

    /* Write files that are changed by calibration factors */
    final GerWriter gerWriter = new GerWriter( m_idManager, rootChannels, channels, m_logger );
    gerWriter.write( m_asciiDirs.channelFile );

    final GebWriter catchmentManager = new GebWriter( m_logger, catchments, naControl, tsFileManager, m_idManager );
    catchmentManager.write( m_asciiDirs.catchmentFile );
  }

  public IStatus getStatus( )
  {
    return m_log.asMultiStatusOrOK( Messages.getString( "NAModellConverter_0" ) ); //$NON-NLS-1$
  }
}
