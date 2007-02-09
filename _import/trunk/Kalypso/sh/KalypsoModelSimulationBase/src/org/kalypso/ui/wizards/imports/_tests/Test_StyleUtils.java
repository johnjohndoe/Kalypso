package org.kalypso.ui.wizards.imports._tests;

import java.awt.Color;
import java.util.HashMap;

import javax.xml.transform.TransformerFactoryConfigurationError;

import junit.framework.TestCase;

import org.kalypso.ui.wizards.imports.utils.StyleUtils;

public class Test_StyleUtils extends TestCase {

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
