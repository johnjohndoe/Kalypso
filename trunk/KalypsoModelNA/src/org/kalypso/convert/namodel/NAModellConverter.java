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
package org.kalypso.convert.namodel;

import java.util.logging.Logger;

import org.apache.commons.io.FileUtils;
import org.kalypso.convert.namodel.manager.AsciiBuffer;
import org.kalypso.convert.namodel.manager.BodenartManager;
import org.kalypso.convert.namodel.manager.BodentypManager;
import org.kalypso.convert.namodel.manager.CatchmentManager;
import org.kalypso.convert.namodel.manager.ChannelManager;
import org.kalypso.convert.namodel.manager.HRBFileWriter;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.convert.namodel.manager.NetFileManager;
import org.kalypso.convert.namodel.manager.NutzungManager;
import org.kalypso.convert.namodel.manager.SchneeManager;
import org.kalypso.convert.namodel.manager.SudsFileWriter;
import org.kalypso.model.hydrology.binding.NAHydrotop;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.Node;
import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.model.hydrology.internal.preprocessing.RelevantNetElements;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.HydroHash;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.HydrotopeWriter;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.LanduseHash;
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

  private final CatchmentManager m_catchmentManager;

  private final ChannelManager m_gerinneManager;

  private final NAConfiguration m_conf;

  private final NetFileManager m_nodeManager;

  private final Logger m_logger;

  public NAModellConverter( final NAConfiguration conf, final Node rootNode, final Logger logger ) throws Exception
  {
    m_conf = conf;
    m_logger = logger;

    m_catchmentManager = new CatchmentManager( m_conf, logger );
    m_gerinneManager = new ChannelManager( m_conf );
    m_nodeManager = new NetFileManager( m_conf, rootNode, logger );
  }

  public void write( ) throws Exception
  {
    // TODO replace this AsciiBuffer with some no-memory-consuming structure (regular StringBuffer), or
    // better: directly write into the files
    final AsciiBuffer asciiBuffer = new AsciiBuffer();

    // FIXME: get as result value from net file writer

    final GMLWorkspace modelWorkspace = m_conf.getModelWorkspace();
    final NaModell naModel = (NaModell) modelWorkspace.getRootFeature();
    final GMLWorkspace synthNWorkspace = m_conf.getSynthNWorkspace();
    final GMLWorkspace parameterWorkspace = m_conf.getParameterWorkspace();
    final Parameter parameter = (Parameter) parameterWorkspace.getRootFeature();
    final NAHydrotop hydrotopeCollection = m_conf.getHydrotopeCollection();
    final GMLWorkspace sudsWorkspace = m_conf.getSudsWorkspace();
    final IDManager idManager = m_conf.getIdManager();

    final RelevantNetElements relevantElements = m_nodeManager.writeFile( asciiBuffer, modelWorkspace, synthNWorkspace );

    // FIXME: write catchment manager separately
    m_catchmentManager.writeFile( relevantElements, asciiBuffer, modelWorkspace );
    m_gerinneManager.writeFile( relevantElements, asciiBuffer, modelWorkspace );

    FileUtils.writeStringToFile( m_conf.getNetFile(), asciiBuffer.getNetBuffer().toString(), null );

    // FIXME: write channel and catchment file spearately
    FileUtils.writeStringToFile( m_conf.getChannelFile(), asciiBuffer.getChannelBuffer().toString(), null );

    FileUtils.writeStringToFile( m_conf.getRHBFile(), asciiBuffer.getRhbBuffer().toString(), null );
    FileUtils.writeStringToFile( m_conf.getZFTFile(), asciiBuffer.getZFTBuffer().toString(), null );

    /* Catchment file */
    FileUtils.writeStringToFile( m_conf.getCatchmentFile(), asciiBuffer.getCatchmentBuffer().toString(), null );

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

    final StringBuffer bodartBuffer = new StringBuffer();
    new BodenartManager().writeFile( bodartBuffer, parameterWorkspace );
    FileUtils.writeStringToFile( m_conf.getBodenartFile(), bodartBuffer.toString(), null );

    final StringBuffer bodtypBuffer = new StringBuffer();
    new BodentypManager().writeFile( bodtypBuffer, parameterWorkspace );
    FileUtils.writeStringToFile( m_conf.getBodentypFile(), bodtypBuffer.toString(), null );

    final StringBuffer snowBuffer = new StringBuffer();
    new SchneeManager().writeFile( snowBuffer, parameterWorkspace );
    FileUtils.writeStringToFile( m_conf.getSchneeFile(), snowBuffer.toString(), null );

    final NutzungManager nutzungManager = new NutzungManager( m_conf.getNutzungDir() );
    nutzungManager.writeFile( parameter, m_hydroHash );

    final SudsFileWriter sudsFileWriter = new SudsFileWriter( naModel, hydrotopeCollection, sudsWorkspace, m_conf.getSwaleAndTrenchFile(), m_logger );
    sudsFileWriter.write();

    final HRBFileWriter hrbFileWriter = new HRBFileWriter( naModel.getStorageChannels(), m_conf, m_logger );
    hrbFileWriter.write();
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
