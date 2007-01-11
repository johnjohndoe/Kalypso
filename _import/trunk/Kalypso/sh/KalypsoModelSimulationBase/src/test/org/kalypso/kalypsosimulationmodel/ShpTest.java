package test.org.kalypso.kalypsosimulationmodel;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;

import junit.framework.TestCase;

import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ShapeToIRoughnessCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.opengis.cs.CS_CoordinateSystem;

public class ShpTest extends TestCase {

	public final void testConvertShp2Gml() throws MalformedURLException {
//		URL outputFileURL = new URL("file:D:/Eclipse/TESTS_RESULTS/shapeConverter.gml");
		URL inputFileURL = new URL("file:D:/Eclipse/Test/Kelling_Stadt/Landuse/landuse.shp");
		//URL inputFileURL = new URL("file:D:/Eclipse/Test/OW_Schwale/Landuse/landuse.shp");
		CS_CoordinateSystem cs = ConvenienceCSFactory.getInstance().getOGCCSByName( "EPSG:31467" );
		//String schemaNamespace = "http://www.tu-harburg.de/wb/kalypso/rma10s/2d";

		try {
			List list = ShapeToIRoughnessCollection.transform2List(inputFileURL, cs, "LANDUSE");
			IRoughnessPolygonCollection collection = ShapeToIRoughnessCollection.transform(inputFileURL, cs, "LANDUSE");
		} catch (GmlSerializeException e) {
			fail("GmlSerializeException");
			e.printStackTrace();
		} catch (Exception e) {
			fail("Exception");
			e.printStackTrace();
		}
	}

}
