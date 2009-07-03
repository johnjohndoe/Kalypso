package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileWriter;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.extension.ITypeRegistry;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.deegree_impl.gml.schema.GMLSchema;
import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.feature.GMLWorkspace_Impl;
import org.kalypso.convert.update.UpdateModell;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.opengis.cs.CS_CoordinateSystem;

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

  public static void main( String[] args )
  {
    try
    {
      // general
      final ITypeRegistry registry = TypeRegistrySingleton.getTypeRegistry();
      registry.registerTypeHandler( new ObservationLinkHandler() );
      //      final File tmpDir = new File( "/tmp/na_tmp" );
      ascii2gml(  );
      //      gml2asciil(tmpDir);
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  public static void gml2asciil( ) throws MalformedURLException, Exception 
  {

    // export
    final File gmlFile = new File( "/home/doemming/weisseElsterGML/naModel.gml" );
    final File asciiBaseDir = FileUtilities.createNewTempDir( "NA_asciiBaseDir" );

    final NAConfiguration conf = NAConfiguration.getGml2AsciiConfiguration( gmlFile.toURL(),
        asciiBaseDir);
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( gmlFile.toURL(), conf
        .getSchemaURL() );
    featureToAscii( conf, workspace );

  }

  public static void ascii2gml( ) throws Exception
  {
    //            Configuration conf = new Configuration(new File("test"));
    final File gmlBaseDir = FileUtilities.createNewTempDir( "NA_gmlBaseDir" );
    NAConfiguration conf = NAConfiguration.getAscii2GmlConfiguration( new File(
        "/home/doemming/weisseElsterUpdate" ), gmlBaseDir );
    Feature rootFeature = asciiToFeature( conf );
    insertGeometries( rootFeature, "/home/doemming/weisseElster/shapes" );
    File gmlFile = new File( gmlBaseDir, "naModel.gml" );
    
    final GMLSchema gmlSchema=new GMLSchema(conf.getSchemaURL());
    GMLWorkspace workspace = new GMLWorkspace_Impl( gmlSchema.getFeatureTypes(),rootFeature,
            null, ":project:.model/schema/namodel.gml");
    GmlSerializer.serializeWorkspace(new FileWriter( gmlFile ),workspace);
    final UpdateModell updater=new UpdateModell(gmlFile.toURL());
    updater.updateIt();
  }

  private static void insertGeometries( Feature modelFeature, String shapeDir )
      throws GmlSerializeException
  {
    // load ShapeFile
    ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
    CS_CoordinateSystem cSystem = org.deegree_impl.model.cs.Adapters.getDefault().export(
        csFac.getCSByName( "EPSG:31468" ) );

    final GMLWorkspace catchmentWorkspace = ShapeSerializer.deserialize( shapeDir + "/ezg_agg2", cSystem, null );
    final List catchmentFeatures = (List)catchmentWorkspace.getRootFeature().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );
    
    final GMLWorkspace channelWorkspace = ShapeSerializer.deserialize( shapeDir + "/river elements",
        cSystem, null );
    final List channelFeatures = (List)channelWorkspace.getRootFeature().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );
    
    final GMLWorkspace nodeWorkspace = ShapeSerializer.deserialize( shapeDir + "/knoten", cSystem, null );
    final List nodeFeatures = (List)nodeWorkspace.getRootFeature().getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );
    
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
    m_schema = new GMLSchema( conf.getSchemaURL() );

    m_catchmentManager = new CatchmentManager( m_schema, m_conf );
    m_gerinneManager = new ChannelManager( m_schema, m_conf );
    m_nodeManager = new NetFileManager( m_conf );
    m_parseManager = new ParseManager( m_schema, conf, m_catchmentManager, m_gerinneManager,
        m_nodeManager );
  }

  public ParseManager getParseManager()
  {
    return m_parseManager;
  }

  public void write( GMLWorkspace workspace ) throws Exception
  {

    Writer writer = new FileWriter( m_conf.getCatchmentFile() );
    m_catchmentManager.writeFile( writer, workspace );
    writer.close();
    Writer writer2 = new FileWriter( m_conf.getChannelFile() );
    m_gerinneManager.writeFile( writer2, workspace );
    writer2.close();

    Writer writer3 = new FileWriter( m_conf.getNetFile() );
    m_nodeManager.writeFile( writer3, workspace );
    writer3.close();

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