package test.org.kalypso.kalypsosimulationmodel;

import javax.xml.transform.TransformerFactoryConfigurationError;

import junit.framework.TestCase;

import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ShapeToIRoughnessCollection;

public class TestCreateStyle extends TestCase {

	protected void setUp() throws Exception {
		super.setUp();
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	public final void testCreateCustomStyle() throws TransformerFactoryConfigurationError, Exception {
		ShapeToIRoughnessCollection.createCustomStyle(
				"D:/Eclipse/TESTS_RESULTS/CustomStyle/test01.gml", 
				"D:/Eclipse/TESTS_RESULTS/CustomStyle/test01.sld", 
				"Naziv stila", 
				null, 
				null, 
				"roughnessID");
//		fail("Not yet implemented");
	}

}
