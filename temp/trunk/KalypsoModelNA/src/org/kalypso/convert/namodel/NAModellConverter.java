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

import java.io.File;
import java.io.FileWriter;
import java.io.Writer;
import java.net.MalformedURLException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.extension.ITypeRegistry;
import org.kalypsodeegree_impl.extension.TypeRegistrySingleton;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCache;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.kalypso.convert.update.UpdateModell;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ogc.gml.typehandler.DiagramTypeHandler;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;

/**
 * import and export of kalypso rainfall runoff models converts between custom
 * ascii format and gml format. importing ascii is always processed into a gml
 * file-structure (includes generating of zml files).
 * 
 * export to ascii can be generated from a gml file or from a gml workspace
 * 
 * @author doemming
 */
public class NAModellConverter
{
  private GMLSchema m_schema;

  private final CatchmentManager m_catchmentManager;

  private final ChannelManager m_gerinneManager;

  private final ParseManager m_parseManager;

  private final NAConfiguration m_conf;

  private final NetFileManager m_nodeManager;

  private final RHBManager m_rhbManager;

  public static void main( String[] args )
  {
    try
    {
      // general
      final ITypeRegistry registry = TypeRegistrySingleton.getTypeRegistry();
      registry.registerTypeHandler( new ObservationLinkHandler() );
      registry.registerTypeHandler( new DiagramTypeHandler() );
      //      final File tmpDir = new File( "/tmp/na_tmp" );
      asciiKollau2gml();
      //      ascii2gml( );
      //      gml2asciil(tmpDir);
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  public static void gml2asciil() throws MalformedURLException, Exception
  {

    // export
    final File gmlFile = new File( "/home/doemming/weisseElsterGML/naModel.gml" );
    final File asciiBaseDir = FileUtilities.createNewTempDir( "NA_asciiBaseDir" );

    final NAConfiguration conf = NAConfiguration.getGml2AsciiConfiguration( gmlFile.toURL(),
        asciiBaseDir );
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( gmlFile.toURL(), conf
        .getSchemaURL() );
    featureToAscii( conf, workspace );

  }

  public static void asciiKollau2gml() throws Exception
  {
    //            Configuration conf = new Configuration(new File("test"));
    final File gmlBaseDir = FileUtilities.createNewTempDir( "NA_gmlBaseDir" );
    NAConfiguration conf = NAConfiguration.getAscii2GmlConfiguration( new File(
        "D:\\Kalypso_NA\\9-Programmtest\\ModellAckermann" ), gmlBaseDir );
    //    /home/doemming/weisseElsterUpdate
    Feature rootFeature = asciiToFeature( conf );
    //    insertGeometries( rootFeature, "/home/doemming/weisseElster/shapes" );
    insertKollauGeometries( rootFeature, "D:\\Kalypso_NA\\9-Programmtest\\ModellAckermann\\shapes" );

    File gmlFile = new File( gmlBaseDir, "naModel.gml" );

    final GMLSchema gmlSchema = GMLSchemaCache.getSchema( conf.getSchemaURL() );
    // TODO: Andreas: Namespace ok for gml?
    // TODO: Andreas: Namespace ok for gml?
    final Document schema = gmlSchema.getSchema();
    final GMLWorkspace workspace = new GMLWorkspace_Impl( gmlSchema.getFeatureTypes(), rootFeature,
        null, ":project:.model/schema/namodel.gml", schema.getNamespaceURI(), gmlSchema
            .getNamespaceMap() );
    GmlSerializer.serializeWorkspace( new FileWriter( gmlFile ), workspace );
    //    final UpdateModell updater=new UpdateModell(gmlFile.toURL());
    //    updater.updateIt();
  }

  public static void ascii2gml() throws Exception
  {
    //            Configuration conf = new Configuration(new File("test"));
    final File gmlBaseDir = FileUtilities.createNewTempDir( "NA_gmlBaseDir" );
    NAConfiguration conf = NAConfiguration.getAscii2GmlConfiguration( new File(
        "/home/doemming/weisseElsterUpdate" ), gmlBaseDir );
    Feature rootFeature = asciiToFeature( conf );
    insertGeometries( rootFeature, "/home/doemming/weisseElster/shapes" );
    File gmlFile = new File( gmlBaseDir, "naModel.gml" );

    final GMLSchema gmlSchema = GMLSchemaCache.getSchema( conf.getSchemaURL() );
    // TODO: Andreas: Namespace ok for gml?
    // TODO: Andreas: Namespace ok for gml?
    final Document schema = gmlSchema.getSchema();
    final GMLWorkspace workspace = new GMLWorkspace_Impl( gmlSchema.getFeatureTypes(), rootFeature,
        null, ":project:.model/schema/namodel.gml", schema.getNamespaceURI(), gmlSchema
            .getNamespaceMap() );
    GmlSerializer.serializeWorkspace( new FileWriter( gmlFile ), workspace );
    final UpdateModell updater = new UpdateModell( gmlFile.toURL() );
    updater.updateIt();
  }

  private static void insertGeometries( Feature modelFeature, String shapeDir )
      throws GmlSerializeException
  {
    // load ShapeFile
    ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
    CS_CoordinateSystem cSystem = org.kalypsodeegree_impl.model.cs.Adapters.getDefault().export(
        csFac.getCSByName( "EPSG:31468" ) );

    final GMLWorkspace catchmentWorkspace = ShapeSerializer.deserialize( shapeDir + "/ezg_agg2",
        cSystem, null );
    final List catchmentFeatures = (List)catchmentWorkspace.getRootFeature().getProperty(
        ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    final GMLWorkspace channelWorkspace = ShapeSerializer.deserialize(
        shapeDir + "/river elements", cSystem, null );
    final List channelFeatures = (List)channelWorkspace.getRootFeature().getProperty(
        ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    final GMLWorkspace nodeWorkspace = ShapeSerializer.deserialize( shapeDir + "/knoten", cSystem,
        null );
    final List nodeFeatures = (List)nodeWorkspace.getRootFeature().getProperty(
        ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    // insertGeometries

    System.out.println( "inserting geometries: catchments" );
    Feature catchmentCollection = (Feature)modelFeature.getProperty( "CatchmentCollectionMember" );
    List catchmentList = (List)catchmentCollection.getProperty( "catchmentMember" );
    copyProperties( catchmentFeatures, "GEOM", "TG_KEN", (Feature[])catchmentList
        .toArray( new Feature[catchmentList.size()] ), "Ort", "inum" );

    System.out.println( "inserting geometries: channels" );
    Feature channelCollection = (Feature)modelFeature.getProperty( "ChannelCollectionMember" );
    List channelList = (List)channelCollection.getProperty( "channelMember" );
    copyProperties( channelFeatures, "GEOM", "RIVER_NO_", (Feature[])channelList
        .toArray( new Feature[channelList.size()] ), "Ort", "inum" );

    System.out.println( "inserting geometries: nodes" );
    Feature nodeCollection = (Feature)modelFeature.getProperty( "NodeCollectionMember" );
    List nodeList = (List)nodeCollection.getProperty( "nodeMember" );
    copyProperties( nodeFeatures, "GEOM", "KNOTEN_NUM", (Feature[])nodeList
        .toArray( new Feature[nodeList.size()] ), "Ort", "num" );
  }

  private static void insertKollauGeometries( Feature modelFeature, String shapeDir )
      throws GmlSerializeException
  {
    // load ShapeFile
    ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
    CS_CoordinateSystem cSystem = org.kalypsodeegree_impl.model.cs.Adapters.getDefault().export(
        csFac.getCSByName( "EPSG:31467" ) );

    final GMLWorkspace catchmentWorkspace = ShapeSerializer.deserialize( shapeDir
        + "\\Teileinzugsgebiete", cSystem, null );
    final List catchmentFeatures = (List)catchmentWorkspace.getRootFeature().getProperty(
        ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    final GMLWorkspace channelWorkspace = ShapeSerializer.deserialize( shapeDir + "\\straenge",
        cSystem, null );
    final List channelFeatures = (List)channelWorkspace.getRootFeature().getProperty(
        ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    final GMLWorkspace nodeWorkspace = ShapeSerializer.deserialize( shapeDir + "\\modellknoten",
        cSystem, null );
    final List nodeFeatures = (List)nodeWorkspace.getRootFeature().getProperty(
        ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    // insertGeometries

    System.out.println( "inserting geometries: catchments" );
    Feature catchmentCollection = (Feature)modelFeature.getProperty( "CatchmentCollectionMember" );
    List catchmentList = (List)catchmentCollection.getProperty( "catchmentMember" );
    copyProperties( catchmentFeatures, "GEOM", "TEILGEBNR", (Feature[])catchmentList
        .toArray( new Feature[catchmentList.size()] ), "Ort", "inum" );

    System.out.println( "inserting geometries: channels" );
    Feature channelCollection = (Feature)modelFeature.getProperty( "ChannelCollectionMember" );
    List channelList = (List)channelCollection.getProperty( "channelMember" );
    copyProperties( channelFeatures, "GEOM", "ID", (Feature[])channelList
        .toArray( new Feature[channelList.size()] ), "Ort", "inum" );

    System.out.println( "inserting geometries: nodes" );
    Feature nodeCollection = (Feature)modelFeature.getProperty( "NodeCollectionMember" );
    List nodeList = (List)nodeCollection.getProperty( "nodeMember" );
    copyProperties( nodeFeatures, "GEOM", "KNOTENR", (Feature[])nodeList
        .toArray( new Feature[nodeList.size()] ), "Ort", "num" );
  }

  private static void copyProperties( final List catchmentFeatures, String orgGeomPropName,
      String orgIdPropName, Feature[] destFE, String destGeomPropName, String destIdPropName )
  {
    HashMap orgHash = new HashMap();
    for( Iterator iter = catchmentFeatures.iterator(); iter.hasNext(); )
    {
      final Feature f = (Feature)iter.next();
      String id = f.getProperty( orgIdPropName ).toString();
      orgHash.put( id, f );
    }
    for( int i = 0; i < destFE.length; i++ )
    {
      Feature destFeature = destFE[i];
      String id = destFeature.getProperty( destIdPropName ).toString();
      //            System.out.println("processing id=" + id);
      Feature orgFeaure = (Feature)orgHash.get( id );
      if( orgFeaure != null )
      {
        Object value = orgFeaure.getProperty( orgGeomPropName );
        if( value == null )
          System.out.println( "copyvalue is null: id=" + id );
        FeatureProperty fProp = FeatureFactory.createFeatureProperty( destGeomPropName, value );
        destFeature.setProperty( fProp );
      }
      else
        System.out.println( "not found in shapeFile: id=" + id );
    }
  }

  public NAModellConverter( NAConfiguration conf ) throws Exception
  {
    m_conf = conf;
    m_schema = GMLSchemaCache.getSchema( conf.getSchemaURL() );

    m_catchmentManager = new CatchmentManager( m_schema, m_conf );
    m_gerinneManager = new ChannelManager( m_schema, m_conf );
    m_nodeManager = new NetFileManager( m_conf );
    m_rhbManager = new RHBManager( m_schema, m_conf );
    m_parseManager = new ParseManager( m_schema, conf, m_catchmentManager, m_gerinneManager,
        m_nodeManager, m_rhbManager );
  }

  public ParseManager getParseManager()
  {
    return m_parseManager;
  }

  public void write( GMLWorkspace workspace ) throws Exception
  {
    AsciiBuffer asciiBuffer = new AsciiBuffer();

    m_nodeManager.writeFile( asciiBuffer, workspace );
    m_catchmentManager.writeFile( asciiBuffer, workspace );
    m_gerinneManager.writeFile( asciiBuffer, workspace );

    Writer writer3 = new FileWriter( m_conf.getNetFile() );
    writer3.write( asciiBuffer.getNetBuffer().toString() );
    writer3.close();

    Writer writer = new FileWriter( m_conf.getCatchmentFile() );
    writer.write( asciiBuffer.getCatchmentBuffer().toString() );
    writer.close();

    Writer writer2 = new FileWriter( m_conf.getChannelFile() );
    writer2.write( asciiBuffer.getChannelBuffer().toString() );
    writer2.close();

    Writer writer4 = new FileWriter( m_conf.getRHBFile() );
    writer4.write( asciiBuffer.getRhbBuffer().toString() );
    writer4.close();
  }

  public static Feature asciiToFeature( NAConfiguration conf ) throws Exception
  {
    NAModellConverter main = new NAModellConverter( conf );
    return main.getParseManager().asciiToFeature();
  }

  public static void featureToAscii( NAConfiguration conf, GMLWorkspace workspace )
      throws Exception
  {
    NAModellConverter main = new NAModellConverter( conf );
    main.write( workspace );
  }

  //    public static Writer getWriter(URL url) throws IOException
  //    {
  //        URLConnection connection = url.openConnection();
  //        connection.setDoOutput(true);
  //
  //        connection.getOutputStream();
  //        return new OutputStreamWriter(connection.getOutputStream());
  //
  //    }
}