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
import org.kalypso.convert.gml2core.SudsFileWriter;
import org.kalypso.convert.namodel.manager.AsciiBuffer;
import org.kalypso.convert.namodel.manager.BodenartManager;
import org.kalypso.convert.namodel.manager.BodentypManager;
import org.kalypso.convert.namodel.manager.CatchmentManager;
import org.kalypso.convert.namodel.manager.ChannelManager;
import org.kalypso.convert.namodel.manager.HydrotopManager;
import org.kalypso.convert.namodel.manager.IdleLanduseManager;
import org.kalypso.convert.namodel.manager.NetFileManager;
import org.kalypso.convert.namodel.manager.NutzungManager;
import org.kalypso.convert.namodel.manager.ParseManager;
import org.kalypso.convert.namodel.manager.SchneeManager;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * import and export of kalypso rainfall runoff models converts between custom ascii format and gml format. importing
 * ascii is always processed into a gml file-structure (includes generating of zml files). export to ascii can be
 * generated from a gml file or from a gml workspace
 * 
 * @author doemming
 */
public class NAModellConverter
{
  private final GMLSchema m_modelSchema;

  private final CatchmentManager m_catchmentManager;

  private final ChannelManager m_gerinneManager;

  private final ParseManager m_parseManager;

  private final NAConfiguration m_conf;

  private final NetFileManager m_nodeManager;

  private final HydrotopManager m_hydrotopManager;

  private final BodenartManager m_bodartManager;

  private final BodentypManager m_bodtypManager;

  private final NutzungManager m_nutzManager;

  private final SchneeManager m_schneeManager;

  private final IdleLanduseManager m_idleLanduseManager;

  public NAModellConverter( final NAConfiguration conf, final String rootNodeID, final Logger logger ) throws Exception
  {
    m_conf = conf;

    final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
    m_modelSchema = schemaCatalog.getSchema( NaModelConstants.NS_NAMODELL, (String) null );
    final GMLSchema m_parameterSchema = schemaCatalog.getSchema( NaModelConstants.NS_NAPARAMETER, (String) null );

    m_catchmentManager = new CatchmentManager( m_modelSchema, m_conf, logger );
    m_gerinneManager = new ChannelManager( m_modelSchema, m_conf );
    m_nodeManager = new NetFileManager( m_conf, rootNodeID, logger );
    m_hydrotopManager = new HydrotopManager( m_conf, logger );
    m_bodartManager = new BodenartManager( m_parameterSchema, m_conf );
    m_bodtypManager = new BodentypManager( m_parameterSchema, m_conf );
    m_nutzManager = new NutzungManager( m_parameterSchema, m_conf );
    m_schneeManager = new SchneeManager( m_parameterSchema, m_conf );
    m_idleLanduseManager = new IdleLanduseManager( m_parameterSchema, m_conf );
    m_parseManager = new ParseManager( m_parameterSchema, conf, m_bodartManager, m_bodtypManager, m_nutzManager, m_schneeManager, m_idleLanduseManager );
  }

  public ParseManager getParseManager( )
  {
    return m_parseManager;
  }

  public void write( ) throws Exception
  {
    // TODO replace this AsciiBuffer with some no-memory-consuming structure (regular StringBuffer)
    final AsciiBuffer asciiBuffer = new AsciiBuffer();

    final GMLWorkspace modelWorkspace = m_conf.getModelWorkspace();
    final GMLWorkspace synthNWorkspace = m_conf.getSynthNWorkspace();
    final GMLWorkspace parameterWorkspace = m_conf.getParameterWorkspace();
    final GMLWorkspace hydrotopeWorkspace = m_conf.getHydrotopeWorkspace();

    m_nodeManager.writeFile( asciiBuffer, modelWorkspace, synthNWorkspace );
    m_catchmentManager.writeFile( asciiBuffer, modelWorkspace );
    m_gerinneManager.writeFile( asciiBuffer, modelWorkspace );

    FileUtils.writeStringToFile( m_conf.getNetFile(), asciiBuffer.getNetBuffer().toString(), null );
    FileUtils.writeStringToFile( m_conf.getCatchmentFile(), asciiBuffer.getCatchmentBuffer().toString(), null );
    FileUtils.writeStringToFile( m_conf.getChannelFile(), asciiBuffer.getChannelBuffer().toString(), null );
    FileUtils.writeStringToFile( m_conf.getRHBFile(), asciiBuffer.getRhbBuffer().toString(), null );
    FileUtils.writeStringToFile( m_conf.getZFTFile(), asciiBuffer.getZFTBuffer().toString(), null );

    if( hydrotopeWorkspace != null )
    {
      m_hydrotopManager.writeFile( asciiBuffer, hydrotopeWorkspace, modelWorkspace, parameterWorkspace );
      FileUtils.writeStringToFile( m_conf.getHydrotopFile(), asciiBuffer.getHydBuffer().toString(), null );

      // generate ascii mapping
      final StringBuffer buffer = new StringBuffer();
      for( final String line : m_conf.getHydrotopMapping() )
        buffer.append( line ).append( "\n" ); //$NON-NLS-1$
      FileUtils.writeStringToFile( m_conf.getHydrotopMappingFile(), buffer.toString(), null );
    }

    if( parameterWorkspace != null )
    {
      final StringBuffer bodartBuffer = new StringBuffer();
      m_bodartManager.writeFile( bodartBuffer, parameterWorkspace );
      FileUtils.writeStringToFile( m_conf.getBodenartFile(), bodartBuffer.toString(), null );

      final StringBuffer bodtypBuffer = new StringBuffer();
      m_bodtypManager.writeFile( bodtypBuffer, parameterWorkspace );
      FileUtils.writeStringToFile( m_conf.getBodentypFile(), bodtypBuffer.toString(), null );

      final StringBuffer snowBuffer = new StringBuffer();
      m_schneeManager.writeFile( snowBuffer, parameterWorkspace );
      FileUtils.writeStringToFile( m_conf.getSchneeFile(), snowBuffer.toString(), null );

      m_nutzManager.writeFile( parameterWorkspace );
    }

    new SudsFileWriter( m_conf ).write();
  }

  public static Feature parameterAsciiToFeature( final NAConfiguration conf, final Logger logger ) throws Exception
  {
    // Root node always null, it is not used in the parseManager
    final NAModellConverter main = new NAModellConverter( conf, null, logger );
    return main.getParseManager().parameterAsciiToFeature();
  }
}