package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.URL;
import java.net.URLConnection;
import java.util.HashMap;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.gml.schema.GMLSchema;
import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.deegree_impl.model.feature.FeatureFactory;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author doemming
 */
public class Main
{
    private GMLSchema m_schema;

    private final CatchmentManager m_catchmentManager;

    private final ChannelManager m_gerinneManager;

    private final ParseManager m_parseManager;

    private Configuration m_conf;

    public static void main(String[] args)
    {
        try {
            Configuration conf = new Configuration(new File("test"));
            Feature fe = asciiToFeature(conf);
            File gmlFile = File.createTempFile("namodell_test", ".gml");
            insertGeometries(fe);
            GmlSerializer.serializeFeature(new FileWriter(gmlFile), fe, null);

            GMLWorkspace workspace = GmlSerializer.createGMLWorkspace(gmlFile
                    .toURL(), conf.getSchemaURL());
            Configuration conf2 = new Configuration(new File("/tmp"));
            //    	writeGML(conf2);// fe1,
            // "../../../../../data/out/out.xml",schemaLocation);
            featureToAscii(conf2, workspace.getRootFeature());//fe3,schemaLocation);

            //    	Configuration conf3=new Configuration("test/output/",null);
            //    	Feature fe3=readGML(conf3);
            //  
            //    	
            //    	Configuration conf4=new Configuration("test/output2/",fe3);
            //    	featureToAscii(conf4);//fe3,schemaLocation);
            //    	writeGML( conf4);//fe2,
            // "../../../../../data/out/out2.xml",schemaLocation );
            //    	Feature fe4 = readGML(conf4);//
            // "../../../../../data/out/out2.xml",schemaLocation );
            //      
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void insertGeometries(Feature modelFeature)
            throws GmlSerializeException
    {
        // load ShapeFile
        ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
        CS_CoordinateSystem cSystem = org.deegree_impl.model.cs.Adapters
                .getDefault().export(csFac.getCSByName("EPSG:4326"));
        // TODO richtiges EPSG waehlen
        KalypsoFeatureLayer layer = ShapeSerializer.deserialize(
                "data/shapes/ezg_agg2", cSystem, cSystem, null);
        Feature[] orgFEs = layer.getAllFeatures();// insertGeometries

        Feature fe2 = (Feature) modelFeature
                .getProperty("CatchmentCollectionMember");
        List list = (List) fe2.getProperty("catchmentMember");
        copyProperties(orgFEs, "GEOM", "TG_KEN", (Feature[]) list
                .toArray(new Feature[list.size()]), "Ort", "inum");
    }

    private static void copyProperties(Feature[] orgFE, String orgGeomPropName,
            String orgIdPropName, Feature[] destFE, String destGeomPropName,
            String destIdPropName)
    {
        HashMap orgHash = new HashMap();
        for (int i = 0; i < orgFE.length; i++) {
            String id = orgFE[i].getProperty(orgIdPropName).toString();
            orgHash.put(id, orgFE[i]);
        }
        for (int i = 0; i < destFE.length; i++) {
            Feature destFeature = destFE[i];
            String id = destFeature.getProperty(destIdPropName).toString();
            System.out.println("processing id=" + id);
            Feature orgFeaure = (Feature) orgHash.get(id);
            if (orgFeaure != null) {
                Object value = orgFeaure.getProperty(orgGeomPropName);
                if (value == null)
                    System.out.println("copyvalue is null: id=" + id);
                FeatureProperty fProp = FeatureFactory.createFeatureProperty(
                        destGeomPropName, value);
                destFeature.setProperty(fProp);
            } else
                System.out.println("not found in shapeFile: id=" + id);
        }
    }

    public Main(Configuration conf) throws Exception
    {
        m_conf = conf;
        //    m_schemaURL=getClass().getResource(schemaLocation);
        m_schema = new GMLSchema(conf.getSchemaURL());

        m_catchmentManager = new CatchmentManager(m_schema, m_conf);
        m_gerinneManager = new ChannelManager(m_schema, m_conf);
        m_parseManager = new ParseManager(m_schema, conf, m_catchmentManager,
                m_gerinneManager);
    }

    //  public Main( String schemaLocation,Feature feature ) throws Exception
    //  {
    //  	 m_schemaURL=getClass().getResource(schemaLocation);
    //     m_schema = new GMLSchema(m_schemaURL );
    //  
    //     m_catchmentManager = new CatchmentManager( m_schema, new File(
    // "src/WernerCatchment.txt" ),
    //        feature );
    //     m_gerinneManager = new ChannelManager( m_schema, new File(
    // "src/gerinne.txt" ), feature );
    //     m_parseManager = new ParseManager( m_schema, m_catchmentManager,
    // m_gerinneManager );
    //  }

    public ParseManager getParseManager()
    {
        return m_parseManager;
    }

    public void write(Feature rootFeature) throws IOException
    {

        Writer writer = new FileWriter(m_conf.getCatchmentFile());// new File(
                                                                  // "data/out/we_nat.geb_out"
                                                                  // ) );
        m_catchmentManager.writeFile(writer, rootFeature);
        writer.close();
        Writer writer2 = new FileWriter(m_conf.getChannelFile());
        m_gerinneManager.writeFile(writer2, rootFeature);
        writer2.close();
    }

    //  public void toGML( )
    //  {
    //  	Writer writer = null;
    //    try
    //    {
    //    	URLConnection connection = m_conf.getModellURL().openConnection();
    //		connection.setDoOutput(true);
    //		connection.getOutputStream();
    //		 writer=new OutputStreamWriter(connection.getOutputStream());
    //  	
    //      GmlSerializer.serializeFeature( writer, m_conf.getRootFeature(), null );
    //    }
    //    catch( Exception e )
    //    {
    //      e.printStackTrace();
    //    }
    //    finally
    //    {
    //      try
    //      {
    //        writer.close();
    //      }
    //      catch( IOException e1 )
    //      {
    //        e1.printStackTrace();
    //      }
    //    }
    //  }

    //  public Feature loadGML()
    //  {
    //    Feature feature = null;
    //    Reader reader;
    //    try
    //    {
    //      feature =
    // GmlSerializer.deserializeFeature(m_conf.getModellURL(),m_conf.getSchemaURL()
    // );
    //      System.out.println( " loaded GML " );
    //    }
    //    catch( Exception e )
    //    {
    //      e.printStackTrace();
    //    }
    //    return feature;
    //  }

    //  public static Feature readGML( Configuration conf ) throws Exception
    //  {
    //    Main main = new Main(conf);
    //    return main.loadGML();
    //  }

    public static Feature asciiToFeature(Configuration conf) throws Exception
    {
        Main main = new Main(conf);
        return main.getParseManager().asciiToFeature();
    }

    public static void featureToAscii(Configuration conf, Feature rootFeature)
            throws Exception
    {
        Main main = new Main(conf);
        main.write(rootFeature);
    }

    //  public static void writeGML(Configuration conf) throws Exception//f
    // Feature feature, String gmlLocation,String schemaLocation ) throws
    // Exception
    //  {
    //    Main main = new Main( conf );
    //    main.toGML();
    //  }

    public static Writer getWriter(URL url) throws IOException
    {
        URLConnection connection = url.openConnection();
        connection.setDoOutput(true);

        connection.getOutputStream();
        return new OutputStreamWriter(connection.getOutputStream());

    }
}