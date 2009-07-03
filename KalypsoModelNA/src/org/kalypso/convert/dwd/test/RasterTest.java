package org.kalypso.convert.dwd.test;

import java.io.File;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;

import javax.xml.bind.Marshaller;

import junit.framework.TestCase;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.extension.ITypeRegistry;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.kalypso.convert.dwd.KrigingReader;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.zml.repository.virtual.ObjectFactory;
import org.kalypso.zml.repository.virtual.VirtualRepository;

public class RasterTest extends TestCase
{
    public void testRaster()
    {
        KrigingReader reader = null;
        InputStream resourceAsStream = getClass().getResourceAsStream(
                "../resources/Kriging_GewichteWeisseElster.txt");
        try
        {
            final ITypeRegistry registry = TypeRegistrySingleton
                    .getTypeRegistry();
            registry.registerTypeHandler(new ObservationLinkHandler());
            final InputStreamReader inputStreamReader = new InputStreamReader(
                    resourceAsStream);
            reader = new KrigingReader(inputStreamReader);
            FileWriter gmlWriter = new FileWriter(new File("/tmp/model.conf"));
            URL gmlURL = getClass().getResource(
                    "../../namodel/modell/modell.gml");
            URL schemaURL = getClass().getResource(
                    "../../namodel/schema/namodellV6.xsd");
            GMLWorkspace workspace = GmlSerializer.createGMLWorkspace(gmlURL,
                    schemaURL);
            Feature[] features = workspace.getFeatures(workspace
                    .getFeatureType("Catchment"));
            final String geoPropName = "Ort";
            //            VirtualRepository repository = reader.createRepositoryConf(
            //                   new
            // Feature[]{features[0],features[2],features[3],features[4],features[5]},
            // geoPropName);
            VirtualRepository repository = reader.createRepositoryConf(
                    features, geoPropName);
            ObjectFactory o = new ObjectFactory();
            Marshaller marshaller = o.createMarshaller();
            FileWriter confWriter = new FileWriter(new File(
                    "/tmp/repository.conf"));
            //            marshaller.setProperty("{http://xml.apache.org/xslt}indent-amount",
            //                    "2");
            //            marshaller.setProperty(OutputKeys.INDENT, "yes");
            marshaller.marshal(repository, confWriter);
            confWriter.close();
        } catch (Exception e)
        {
            e.printStackTrace();
            fail(e.getMessage());
        }
    }
}