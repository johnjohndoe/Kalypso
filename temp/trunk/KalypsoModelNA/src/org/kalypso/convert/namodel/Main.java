package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.Writer;
import java.util.HashMap;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree_impl.gml.schema.GMLSchema;
import org.deegree_impl.gml.schema.XMLHelper;
import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.deegree_impl.model.feature.FeatureFactory;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;

/**
 * @author doemming
 */
public class Main
{
  GMLSchema m_schema;

  private final CatchmentManager m_catchmentManager;

  private final ChannelManager m_gerinneManager;

  private final ParseManager m_parseManager;

  public static void main( String[] args )
  {
    try
    {
      Feature fe1 = asciiToFeature();
      insertGeometries(fe1);
      writeGML( fe1, "data/out/out.xml" );
           
      Feature fe2=readGML("data/out/out.xml");
      writeGML( fe2, "data/out/out2.xml" );
      
      Feature fe3 = readGML( "data/out/out2.xml" );
      
      featureToAscii(fe3);
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  private static void insertGeometries( Feature modelFeature ) throws GmlSerializeException
  {
    // load ShapeFile
    ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
    CS_CoordinateSystem cSystem = org.deegree_impl.model.cs.Adapters.getDefault().export(
        csFac.getCSByName( "EPSG:4326" ) );
    // TODO richtiges EPSG waehlen
    KalypsoFeatureLayer layer = ShapeSerializer.deserialize( "data/shapes/ezg_agg2", cSystem,
        cSystem, null );
    Feature[] orgFEs = layer.getAllFeatures();// insertGeometries
    
    Feature fe2 = (Feature)modelFeature.getProperty( "CatchmentCollectionMember" );
    List list = (List)fe2.getProperty( "catchmentMember" );
    copyProperties( orgFEs, "GEOM", "TG_KEN", (Feature[])list.toArray( new Feature[list.size()] ),
        "Ort", "inum" );
  }

  private static void copyProperties( Feature[] orgFE, String orgGeomPropName,
      String orgIdPropName, Feature[] destFE, String destGeomPropName, String destIdPropName )
  {
    HashMap orgHash = new HashMap();
    for( int i = 0; i < orgFE.length; i++ )
    {
      String id = orgFE[i].getProperty( orgIdPropName ).toString();
      orgHash.put( id, orgFE[i] );
    }
    for( int i = 0; i < destFE.length; i++ )
    {
      Feature destFeature = destFE[i];
      String id = destFeature.getProperty( destIdPropName ).toString();
      System.out.println( "processing id=" + id );
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

  public Main() throws Exception
  {
    String schemaLocation = "../../../../schema/namodellV3.xsd";
    //    +
    //        "../../../../../data/schema/namodellV3.xsd";
    //File m_xsdFile = new File( schemaLocation );
    InputStream schemaStream = getClass().getResourceAsStream( schemaLocation );
    Document dom = XMLHelper.getAsDOM( schemaStream );
    m_schema = new GMLSchema( dom );
    m_catchmentManager = new CatchmentManager( m_schema, new File( "src/WernerCatchment.txt" ),
        null );
    m_gerinneManager = new ChannelManager( m_schema, new File( "src/gerinne.txt" ), null );
    m_parseManager = new ParseManager( m_schema, m_catchmentManager, m_gerinneManager );
  }

  public Main( Feature feature ) throws Exception
  {
    String schemaLocation = "../../../../schema/namodellV3.xsd";
    InputStream schemaStream = getClass().getResourceAsStream( schemaLocation );
    Document dom = XMLHelper.getAsDOM( schemaStream );
    m_schema = new GMLSchema( dom );
    m_catchmentManager = new CatchmentManager( m_schema, new File( "src/WernerCatchment.txt" ),
        feature );
    m_gerinneManager = new ChannelManager( m_schema, new File( "src/gerinne.txt" ), feature );
    m_parseManager = new ParseManager( m_schema, m_catchmentManager, m_gerinneManager );
  }

  public ParseManager getParseManager()
  {
    return m_parseManager;
  }

  public void write() throws IOException
  {
    Writer writer1 = new FileWriter( new File( "data/out/we_nat.geb_out" ) );
    m_catchmentManager.writeFile( writer1 );
    writer1.close();
    Writer writer2 = new FileWriter( new File( "data/out/we_nat.ger_out" ) );
    m_gerinneManager.writeFile( writer2 );
    writer2.close();
  }

  public void toGML( Feature feature, String path )
  {
    Writer writer = null;
    try
    {
      writer = new FileWriter( new File( path ) );
      GmlSerializer.serializeFeature( writer, feature, null );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      try
      {
        writer.close();
      }
      catch( IOException e1 )
      {
        e1.printStackTrace();
      }
    }
  }

  public Feature loadGML( String path )
  {
    Feature feature = null;
    Reader reader;
    try
    {
      reader = new FileReader( new File( path ) );
      feature = GmlSerializer.deserializeFeature( reader, m_schema );
      System.out.println( " loaded GML " );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    return feature;
  }

  public static Feature readGML( String path ) throws Exception
  {
    Main main = new Main();
    return main.loadGML( path );
  }

  public static Feature asciiToFeature() throws Exception
  {
    Main main = new Main();
    return main.getParseManager().asciiToFeature();
  }

  public static void featureToAscii( Feature feature ) throws Exception
  {
    Main main = new Main( feature );
    main.write();
  }

  public static void writeGML( Feature feature, String path ) throws Exception
  {
    Main main = new Main( feature );
    main.toGML( feature, path );
  }
}