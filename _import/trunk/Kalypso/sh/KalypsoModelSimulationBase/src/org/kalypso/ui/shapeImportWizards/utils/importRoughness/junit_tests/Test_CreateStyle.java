package org.kalypso.ui.shapeImportWizards.utils.importRoughness.junit_tests;

import java.awt.Color;
import java.util.HashMap;

import javax.xml.transform.TransformerFactoryConfigurationError;

import junit.framework.TestCase;

import org.kalypso.ui.shapeImportWizards.utils.StyleUtils;

public class Test_CreateStyle extends TestCase {

	protected void setUp() throws Exception {
		super.setUp();
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	public final void testCreateCustomStyle() throws TransformerFactoryConfigurationError, Exception {
		HashMap<String, Color> knownPropertyColorSet = new HashMap<String, Color>();
		knownPropertyColorSet.put("See", 		Color.BLUE);
		knownPropertyColorSet.put("Grasland", 	Color.GREEN);
		
		StyleUtils.createCustomStyle(
				"D:/Eclipse/TESTS_RESULTS/CustomStyle/test01.gml", 
				"D:/Eclipse/TESTS_RESULTS/CustomStyle/test01.sld", 
				"Naziv stila", 
				null, 
				"roughnessID", 
				knownPropertyColorSet);
//		fail("Not yet implemented");
	}

}
