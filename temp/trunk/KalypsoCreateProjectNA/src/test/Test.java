/*
 * Created on Feb 10, 2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package test;

import javax.xml.bind.JAXBException;

import org.deegree.model.feature.*;
import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.opengis.cs.CS_CoordinateSystem;
import org.deegree_impl.extension.*;
import org.kalypso.ogc.sensor.deegree.*;

/**
 * @author Tejas Doshi
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class Test {
	public static final String LOG_FILE = "d://temp//hydroGenLog.log";

	public static final String OUT_FILE = "d://temp//hydrotop.out";

	public static final String SHP_FILE_BASE = "D://Daten//DataForCK//KalypsoQM//GIS//hydrotope_qm";//"d://Kellinghusen//shp//pegel_hydrologie";//"d://hydrotopesdata//hydrotope_qm";

	final static ITypeRegistry registry = TypeRegistrySingleton.getTypeRegistry();

	public static void main(String[] args) {
		ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
		CS_CoordinateSystem cs = org.deegree_impl.model.cs.Adapters
				.getDefault().export(csFac.getCSByName("EPSG:31467"));

		GMLWorkspace gmlWS = null;
		try {
			gmlWS = ShapeSerializer.deserialize(SHP_FILE_BASE, cs, null);
		} catch (GmlSerializeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
//		try {
//			registry.registerTypeHandler(new ObservationLinkHandler());
//		} catch (TypeRegistryException e1) {
//			// TODO Auto-generated catch block
//			e1.printStackTrace();
//		} catch (JAXBException e1) {
//			// TODO Auto-generated catch block
//			e1.printStackTrace();
//		}
		
		Feature rootFeature = gmlWS.getRootFeature();

		FeatureType rootFT = rootFeature.getFeatureType();
		FeatureAssociationTypeProperty ftp = (FeatureAssociationTypeProperty) rootFT
				.getProperty("featureMember");

		FeatureType[] associationFeatureTypes = ftp.getAssociationFeatureTypes();
		FeatureType shapeFT = associationFeatureTypes[0];
		FeatureTypeProperty[] sourceFtp = shapeFT.getProperties();
		
	}
}