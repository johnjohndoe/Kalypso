package org.kalypso.kalypso1d2d.pjt.util;

import static org.junit.Assert.*;

import java.io.IOException;

import javax.xml.bind.JAXBContext;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.jwsdp.JaxbUtilities;
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
	public final void testConvertShp2Gml() {
		//final JAXBContext JC = JaxbUtilities.createQuiet( org.kalypso.template.gismapview.ObjectFactory.class, org.kalypso.template.types.ObjectFactory.class );
		final JAXBContext JC = JaxbUtilities.createQuiet( org.kalypso.zml.obslink.ObjectFactory.class );

		final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
		String filePath = "D:/Eclipse/Test/Kelling_Stadt/Landuse/landuse.shp";
		//String filePath = "D:/Eclipse/Test/OW_Schwale/Landuse/landuse.shp";
		CS_CoordinateSystem cs = ConvenienceCSFactory.getInstance().getOGCCSByName( "EPSG:31467" );
		String schemaNamespace = "http://www.tu-harburg.de/wb/kalypso/rma10s/2d";


		try {
			ImportUtils.convertShp2Gml(filePath, cs, schemaNamespace);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (GmlSerializeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		fail("Not yet implemented"); // TODO
	}

}
