package test.org.kalypso.kalypsosimulationmodel;

import java.io.File;
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
		
		File tempOutPut=null;
		
		try
		{
			tempOutPut= File.createTempFile("test_shp_to_gml", "xml");
			tempOutPut.createNewFile();
		}
		catch (Throwable e) 
		{
			fail(TestUtils.getStackTraceAsString(e));
		}
		
		//URL inputFileURL = new URL("file:D:/Eclipse/Test/OW_Schwale/Landuse/landuse.shp");
		CS_CoordinateSystem cs = 
			ConvenienceCSFactory.getInstance().getOGCCSByName( 
					TestWorkspaces.CS_KEY_GAUSS_KRUEGER);
		//String schemaNamespace = "http://www.tu-harburg.de/wb/kalypso/rma10s/2d";

		try {
//			List list = ShapeToIRoughnessCollection.transform2List(inputFileURL, cs, "LANDUSE");
			IRoughnessPolygonCollection collection = 
				ShapeToIRoughnessCollection.transform(inputFileURL,tempOutPut.toURL(),cs, "LANDUSE");
		} catch (GmlSerializeException e) {
			fail(TestUtils.getStackTraceAsString(e));
		} catch (Exception e) {
			fail(TestUtils.getStackTraceAsString(e));
		}
	}

}
