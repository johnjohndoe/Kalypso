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

import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import org.kalypso.convert.gml2core.SudsFileWriter;
import org.kalypso.convert.namodel.manager.AsciiBuffer;
import org.kalypso.convert.namodel.manager.BodenartManager;
import org.kalypso.convert.namodel.manager.BodentypManager;
import org.kalypso.convert.namodel.manager.CatchmentManager;
import org.kalypso.convert.namodel.manager.ChannelManager;
import org.kalypso.convert.namodel.manager.HydrotopManager;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.convert.namodel.manager.IdleLanduseManager;
import org.kalypso.convert.namodel.manager.NetFileManager;
import org.kalypso.convert.namodel.manager.NutzungManager;
import org.kalypso.convert.namodel.manager.ParseManager;
import org.kalypso.convert.namodel.manager.RHBManager;
import org.kalypso.convert.namodel.manager.SchneeManager;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypsodeegree.model.feature.Feature;

/**
 * import and export of kalypso rainfall runoff models converts between custom ascii format and gml format. importing
 * ascii is always processed into a gml file-structure (includes generating of zml files). export to ascii can be
 * generated from a gml file or from a gml workspace
 * 
 * @author doemming
 */
public class NAModellConverter
{
  private GMLSchema m_modelSchema;

  private final CatchmentManager m_catchmentManager;

  private final ChannelManager m_gerinneManager;

  private final ParseManager m_parseManager;

  private final NAConfiguration m_conf;

  private final NetFileManager m_nodeManager;

  private final RHBManager m_rhbManager;

  private final HydrotopManager m_hydrotopManager;

  private final BodenartManager m_bodartManager;

  private final BodentypManager m_bodtypManager;

  private final NutzungManager m_nutzManager;

  private final SchneeManager m_schneeManager;

//  private final SwaleAndTrenchManager m_swaleAndTrenchManager;

  private final IdleLanduseManager m_idleLanduseManager;

  public NAModellConverter( final NAConfiguration conf ) throws Exception
  {
    m_conf = conf;

    final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
    m_modelSchema = schemaCatalog.getSchema( NaModelConstants.NS_NAMODELL, (String) null );
    GMLSchema m_parameterSchema = schemaCatalog.getSchema( NaModelConstants.NS_NAPARAMETER, (String) null );

    m_catchmentManager = new CatchmentManager( m_modelSchema, m_conf );
    m_gerinneManager = new ChannelManager( m_modelSchema, m_conf );
    m_nodeManager = new NetFileManager( m_conf );
    m_rhbManager = new RHBManager( m_modelSchema, m_conf );
    m_hydrotopManager = new HydrotopManager( m_conf );
//    m_swaleAndTrenchManager = new SwaleAndTrenchManager( m_modelSchema, m_conf );
    m_bodartManager = new BodenartManager( m_parameterSchema, m_conf );
    m_bodtypManager = new BodentypManager( m_parameterSchema, m_conf );
    m_nutzManager = new NutzungManager( m_parameterSchema, m_conf );
    m_schneeManager = new SchneeManager( m_parameterSchema, m_conf );
    m_idleLanduseManager = new IdleLanduseManager( m_parameterSchema, m_conf );
    m_parseManager = new ParseManager( m_modelSchema, m_parameterSchema, conf, m_catchmentManager, m_gerinneManager, m_nodeManager, m_rhbManager, m_bodartManager, m_bodtypManager, m_nutzManager, m_schneeManager, m_idleLanduseManager );
  }

  public ParseManager getParseManager( )
  {
    return m_parseManager;
  }

  public void write( ) throws Exception
  {
    // TODO replace this AsciiBuffer with some no-memory-consuming structure (regular StringBuffer)
    AsciiBuffer asciiBuffer = new AsciiBuffer();

    m_nodeManager.writeFile( asciiBuffer,m_conf.getModelWorkspace(),m_conf.getSynthNWorkspace() );
    m_catchmentManager.writeFile( asciiBuffer, m_conf.getModelWorkspace() );
    m_gerinneManager.writeFile( asciiBuffer, m_conf.getModelWorkspace() );
//    m_swaleAndTrenchManager.writeFile( asciiBuffer, modelWorkspace );
    writeToFile( m_conf.getNetFile(), asciiBuffer.getNetBuffer() );
    writeToFile( m_conf.getCatchmentFile(), asciiBuffer.getCatchmentBuffer() );
    writeToFile( m_conf.getChannelFile(), asciiBuffer.getChannelBuffer() );
    // writeToFile( m_conf.getSwaleAndTrenchFile(), asciiBuffer.getSwaleTrenchBuffer() );
    writeToFile( m_conf.getRHBFile(), asciiBuffer.getRhbBuffer() );
    writeToFile( m_conf.getZFTFile(), asciiBuffer.getZFTBuffer() );

    if( m_conf.getHydrotopeWorkspace() != null )
    {
      m_hydrotopManager.writeFile( asciiBuffer, m_conf.getHydrotopeWorkspace(), m_conf.getModelWorkspace(), m_conf.getParameterWorkspace() );
      writeToFile( m_conf.getHydrotopFile(), asciiBuffer.getHydBuffer() );
      
      
      // generate ascii mapping
      final StringBuffer buffer = new StringBuffer();
      for( final String line : m_conf.getHydrotopMapping() )
        buffer.append( line ).append( "\n" );
      writeToFile( m_conf.getHydrotopMappingFile(), buffer );
      
      
    }

    if( m_conf.getParameterWorkspace() != null )
    {
      m_bodartManager.writeFile( asciiBuffer, m_conf.getParameterWorkspace() );
      m_bodtypManager.writeFile( asciiBuffer, m_conf.getParameterWorkspace() );
      m_schneeManager.writeFile( asciiBuffer, m_conf.getParameterWorkspace() );
      writeToFile( m_conf.getBodenartFile(), asciiBuffer.getBodartBuffer() );
      writeToFile( m_conf.getBodentypFile(), asciiBuffer.getBodtypBuffer() );
      writeToFile( m_conf.getSchneeFile(), asciiBuffer.getSnowBuffer() );
      m_nutzManager.writeFile( m_conf.getParameterWorkspace() );
    }
    
    new SudsFileWriter(m_conf).write();

  }
  public static Feature modelAsciiToFeature( NAConfiguration conf ) throws Exception
  {
    NAModellConverter main = new NAModellConverter( conf );
    return main.getParseManager().modelAsciiToFeature();
  }

  public static Feature parameterAsciiToFeature( NAConfiguration conf ) throws Exception
  {
    NAModellConverter main = new NAModellConverter( conf );
    return main.getParseManager().parameterAsciiToFeature();
  }

  private final void writeToFile( final File file, final StringBuffer buffer ) throws IOException
  {
    final FileOutputStream fileOutputStream = new FileOutputStream( file );
    final DataOutputStream stream = new DataOutputStream( fileOutputStream );
    stream.writeBytes( buffer.toString() );
    stream.close();
  }

// public static void featureToAscii( NAConfiguration conf, GMLWorkspace modelWorkspace, GMLWorkspace
  // parameterWorkspace, GMLWorkspace hydrotopWorkspace, GMLWorkspace synthNWorkspace, final NaNodeResultProvider
  // nodeResultProvider ) throws Exception
// {
// NAModellConverter main = new NAModellConverter( conf );
// main.write( modelWorkspace, parameterWorkspace, hydrotopWorkspace, synthNWorkspace, nodeResultProvider );
// }
}