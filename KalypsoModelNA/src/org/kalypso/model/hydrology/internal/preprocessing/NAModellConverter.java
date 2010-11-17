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

import java.util.logging.Logger;

import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.model.hydrology.binding.NAHydrotop;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.Channel;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.Node;
import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.HydroHash;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.HydrotopeWriter;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.LanduseHash;
import org.kalypso.model.hydrology.internal.preprocessing.writer.BodenartWriter;
import org.kalypso.model.hydrology.internal.preprocessing.writer.BodentypWriter;
import org.kalypso.model.hydrology.internal.preprocessing.writer.GebWriter;
import org.kalypso.model.hydrology.internal.preprocessing.writer.GerWriter;
import org.kalypso.model.hydrology.internal.preprocessing.writer.HRBFileWriter;
import org.kalypso.model.hydrology.internal.preprocessing.writer.NetFileWriter;
import org.kalypso.model.hydrology.internal.preprocessing.writer.NutzungWriter;
import org.kalypso.model.hydrology.internal.preprocessing.writer.RhbWriter;
import org.kalypso.model.hydrology.internal.preprocessing.writer.SchneeManager;
import org.kalypso.model.hydrology.internal.preprocessing.writer.SudsFileWriter;
import org.kalypso.model.hydrology.internal.preprocessing.writer.TimeseriesFileManager;
import org.kalypso.model.hydrology.internal.preprocessing.writer.ZftWriter;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;

/**
 * Import kalypso rainfall runoff models converts between custom ascii format and gml format. Export to ascii can be
 * generated from a gml file or from a gml workspace.
 * 
 * @author doemming
 */
public class NAModellConverter
{
  private HydroHash m_hydroHash;

  private final NAConfiguration m_conf;

  private final Logger m_logger;

  private final Node m_rootNode;

  public NAModellConverter( final NAConfiguration conf, final Node rootNode, final Logger logger ) throws Exception
  {
    m_conf = conf;
    m_rootNode = rootNode;
    m_logger = logger;
  }

  public void write( ) throws Exception
  {
    final GMLWorkspace modelWorkspace = m_conf.getModelWorkspace();
    final NaModell naModel = (NaModell) modelWorkspace.getRootFeature();
    final GMLWorkspace synthNWorkspace = m_conf.getSynthNWorkspace();
    final GMLWorkspace parameterWorkspace = m_conf.getParameterWorkspace();
    final Parameter parameter = (Parameter) parameterWorkspace.getRootFeature();
    final NAHydrotop hydrotopeCollection = m_conf.getHydrotopeCollection();
    final GMLWorkspace sudsWorkspace = m_conf.getSudsWorkspace();
    final IDManager idManager = m_conf.getIdManager();

    final NetFileWriter m_nodeManager = new NetFileWriter( m_conf, m_rootNode, m_logger, modelWorkspace, synthNWorkspace );
    m_nodeManager.write( m_conf.getNetFile() );

    final TimeseriesFileManager tsFileManager = m_nodeManager.getTsFileManager();
    final RelevantNetElements relevantElements = m_nodeManager.getRelevantElements();

    final Integer[] rootChannels = relevantElements.getRootChannels();
    final Channel[] channels = relevantElements.getChannels( idManager );
    final Catchment[] catchments = relevantElements.getCatchments( idManager );

    /* .geb file */
    // FIXME: extract zft stuff
    final GebWriter catchmentManager = new GebWriter( m_conf, m_logger, catchments, naModel, tsFileManager );
    catchmentManager.write( m_conf.getCatchmentFile() );

    final ZftWriter zftWriter = new ZftWriter( idManager, m_logger, catchments );
    zftWriter.write( m_conf.getZFTFile() );

    if( hydrotopeCollection != null )
    {
      // REMARK: initHydroHash must be called after nodeManager.write file has been called, as this marks
      // the features in the ascii buffer to be relevant.
      // TODO: change this bad design: We should just pass a list of catchments to the hydroHash
      final HydroHash hydroHash = initHydroHash( parameter, naModel, hydrotopeCollection, relevantElements );

      final HydrotopeWriter hydrotopManager = new HydrotopeWriter( parameter, idManager, hydroHash, m_logger );
      hydrotopManager.writeHydrotopFile( m_conf.getHydrotopFile(), modelWorkspace, relevantElements );
      hydrotopManager.writeMapping( m_conf.getHydrotopMappingFile() );
    }

    // FIXME: write .ger and .geb file after optimization
    final GerWriter gerWriter = new GerWriter( idManager, rootChannels, channels, m_logger );
    gerWriter.write( m_conf.getChannelFile() );

    final RhbWriter rhbWriter = new RhbWriter( idManager, channels, m_logger );
    rhbWriter.write( m_conf.getRHBFile() );

    final BodenartWriter bodenartManager = new BodenartWriter( parameterWorkspace, m_logger );
    bodenartManager.write( m_conf.getBodenartFile() );

    final BodentypWriter bodentypManager = new BodentypWriter( parameterWorkspace, m_logger );
    bodentypManager.write( m_conf.getBodentypFile() );

    final SchneeManager schneeManager = new SchneeManager( parameterWorkspace, m_logger );
    schneeManager.write( m_conf.getSchneeFile() );

    final NutzungWriter nutzungManager = new NutzungWriter( m_conf.getNutzungDir() );
    nutzungManager.writeFile( parameter, m_hydroHash );

    final SudsFileWriter sudsFileWriter = new SudsFileWriter( naModel, hydrotopeCollection, sudsWorkspace, m_logger );
    sudsFileWriter.write( m_conf.getSwaleAndTrenchFile() );

    final HRBFileWriter hrbFileWriter = new HRBFileWriter( naModel.getStorageChannels(), m_conf, m_logger );
    hrbFileWriter.write( m_conf.getHRBFile() );
  }

  private HydroHash initHydroHash( final Parameter parameter, final NaModell naModel, final NAHydrotop hydrotopeCollection, final RelevantNetElements relevantElements ) throws GM_Exception, SimulationException
  {
    if( m_hydroHash == null )
    {
      final LanduseHash landuseHash = new LanduseHash( parameter, m_logger );
      m_hydroHash = new HydroHash( landuseHash );
      m_hydroHash.initHydrotopes( naModel, hydrotopeCollection, relevantElements );
    }

    return m_hydroHash;
  }

  public HydroHash getHydroHash( )
  {
    return m_hydroHash;
  }

}
