package org.kalypso.kalypso1d2d.pjt.util;

import static org.junit.Assert.fail;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.opengis.cs.CS_CoordinateSystem;

public class Test_ImportUtils {

	@Before
	public void setUp() throws Exception {
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public final void testConvertShp2Gml() throws MalformedURLException {
		URL outputFileURL = new URL("file:D:/Eclipse/TESTS_RESULTS/shapeConverter.gml");
		URL inputFileURL = new URL("file:D:/Eclipse/Test/Kelling_Stadt/Landuse/landuse.shp");
		//URL inputFileURL = new URL("file:D:/Eclipse/Test/OW_Schwale/Landuse/landuse.shp");
		CS_CoordinateSystem cs = ConvenienceCSFactory.getInstance().getOGCCSByName( "EPSG:31467" );
		//String schemaNamespace = "http://www.tu-harburg.de/wb/kalypso/rma10s/2d";
		String schemaNamespace = "http://www.tuhh.de/floodrisk/vectorData";

		try {
			ImportUtils.convertShp2Gml(inputFileURL, cs, schemaNamespace, outputFileURL, "LANDUSE");
		} catch (IOException e) {
			fail("IOException");
			e.printStackTrace();
		} catch (GmlSerializeException e) {
			fail("GmlSerializeException");
			e.printStackTrace();
		}
	}

}
