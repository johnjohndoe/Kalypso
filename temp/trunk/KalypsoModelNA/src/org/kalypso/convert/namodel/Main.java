package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileWriter;
import java.io.Writer;
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

    private NAConfiguration m_conf;

    public static void main(String[] args)
    {
        try {
//            Configuration conf = new Configuration(new File("test"));
            NAConfiguration conf = new NAConfiguration(new File("/home/doemming/weisseElster"));
            Feature fe = asciiToFeature(conf);
            insertGeometries(fe,"/home/doemming/weisseElster/shapes");

            File gmlFile = File.createTempFile("namodell_test", ".gml");
            GmlSerializer.serializeFeature(new FileWriter(gmlFile), fe, null);

            GMLWorkspace workspace = GmlSerializer.createGMLWorkspace(gmlFile
                    .toURL(), conf.getSchemaURL());
            NAConfiguration conf2 = new NAConfiguration(new File("/tmp"));
            featureToAscii(conf2, workspace);//fe3,schemaLocation);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void insertGeometries(Feature modelFeature,String shapeDir)
            throws GmlSerializeException
    {
        // load ShapeFile
        ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
        CS_CoordinateSystem cSystem = org.deegree_impl.model.cs.Adapters
                .getDefault().export(csFac.getCSByName("EPSG:31468"));
//        CS_CoordinateSystem cSystem = org.deegree_impl.model.cs.Adapters
//                .getDefault().export(csFac.getCSByName("EPSG:4326"));
        KalypsoFeatureLayer layer = ShapeSerializer.deserialize(shapeDir+"/ezg_agg2", cSystem, cSystem, null);
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

    public Main(NAConfiguration conf) throws Exception
    {
        m_conf = conf;
        m_schema = new GMLSchema(conf.getSchemaURL());

        m_catchmentManager = new CatchmentManager(m_schema, m_conf);
        m_gerinneManager = new ChannelManager(m_schema, m_conf);
        m_parseManager = new ParseManager(m_schema, conf, m_catchmentManager,
                m_gerinneManager);
    }

   

    public ParseManager getParseManager()
    {
        return m_parseManager;
    }

    public void write(GMLWorkspace workspace) throws Exception
    {

        Writer writer = new FileWriter(m_conf.getCatchmentFile());
        m_catchmentManager.writeFile(writer, workspace);
        writer.close();
        Writer writer2 = new FileWriter(m_conf.getChannelFile());
        m_gerinneManager.writeFile(writer2, workspace);
        writer2.close();
    }

   

    public static Feature asciiToFeature(NAConfiguration conf) throws Exception
    {
        Main main = new Main(conf);
        return main.getParseManager().asciiToFeature();
    }

    public static void featureToAscii(NAConfiguration conf, GMLWorkspace workspace)
            throws Exception
    {
        Main main = new Main(conf);
        main.write(workspace);
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