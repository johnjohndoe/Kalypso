package test.org.kalypso.kalypsosimulationmodel;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

import junit.framework.TestCase;

import org.kalypso.kalypsosimulationmodel.core.util.ImportUtils;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.opengis.cs.CS_CoordinateSystem;

public class ShpTest extends TestCase {

	public final void testConvertShp2Gml() throws MalformedURLException {
		URL outputFileURL = new URL("file:D:/Eclipse/TESTS_RESULTS/shapeConverter.gml");
		URL inputFileURL = new URL("file:D:/Eclipse/Test/Kelling_Stadt/Landuse/landuse.shp");
		//URL inputFileURL = new URL("file:D:/Eclipse/Test/OW_Schwale/Landuse/landuse.shp");
		CS_CoordinateSystem cs = ConvenienceCSFactory.getInstance().getOGCCSByName( "EPSG:31467" );
		//String schemaNamespace = "http://www.tu-harburg.de/wb/kalypso/rma10s/2d";

		try {
			ImportUtils.convertShp2Gml(inputFileURL, cs, outputFileURL, "LANDUSE");
		} catch (IOException e) {
			fail("IOException");
			e.printStackTrace();
		} catch (GmlSerializeException e) {
			fail("GmlSerializeException");
			e.printStackTrace();
		}
	}

}
