package org.kalypso.convert.dwd.test;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;

import junit.framework.TestCase;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.extension.ITypeRegistry;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.kalypso.convert.dwd.KrigingReader;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;

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
            URL gmlURL = getClass().getResource(
                    "../../namodel/modell/modell.gml");
            URL schemaURL = getClass().getResource(
                    "../../namodel/schema/namodellV6.xsd");
            GMLWorkspace workspace = GmlSerializer.createGMLWorkspace(gmlURL,
                    schemaURL);
            Feature[] features = workspace.getFeatures(workspace
                    .getFeatureType("Catchment"));
            final String geoPropName="Ort";
            for (int i = 0; i < features.length; i++)
            {
                Feature feature = features[i];
	            reader.createFilter(feature,geoPropName);
                
            }
        } catch (Exception e)
        {
            e.printStackTrace();
            fail(e.getMessage());
        }
        //
    }

}