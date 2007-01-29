package org.kalypso.ui.shapeImportWizards.utils.importRoughness.junit_tests;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;

import junit.framework.TestCase;

import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ui.shapeImportWizards.utils.importRoughness.TransformerShapeToIRoughnessCollection;

public class ShpTest extends TestCase {

	public final void testConvertShp2Gml() throws MalformedURLException {
//		URL inputFileURL = new URL("file:D:/Eclipse/Test/Kelling_Stadt/Landuse/landuse.shp");
//		URL inputFileURL = new URL("file:D:/Eclipse/Test/OW_Schwale/Landuse/landuse.shp");
		URL inputFileURL = new URL("file:D:/Eclipse/Workspace/KalypsoModelSimulationBase/src/test/org/kalypso/kalypsosimulationmodel/data/rauheitstest.shp");
//		URL outputFileURL = new URL("file:D:/Eclipse/TESTS_RESULTS/shapeConverter.gml");
		URL outputFileURL = new URL("file:D:/Eclipse/TESTS_RESULTS/rauheitstest.gml");
		
//		try {
////			List list = ShapeToIRoughnessCollection.transform2List(inputFileURL, cs, "LANDUSE");
//			ShapeToIRoughnessCollection.transform(inputFileURL, null, "RAUHEITSKL", outputFileURL);
//		} catch (GmlSerializeException e) {
//			fail(TestUtils.getStackTraceAsString(e));
//		} catch (InvocationTargetException e) {
//			fail(TestUtils.getStackTraceAsString(e));
//		} catch (IOException e) {
//			fail(TestUtils.getStackTraceAsString(e));
//		}
	}

}
