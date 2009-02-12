package de.tuhh.wb.javagis.tools;

/*
 * Shape2GML.java
 *
 * Created on 21. Dezember 2002, 20:58
 */

import java.io.*;

import org.deegree.gml.*;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.gml.*;
import org.deegree_impl.io.shpapi.*;
import org.deegree_impl.model.feature.*;
//import org.deegree_impl.tools.Debug;

/**
 *
 * @author  <a href="poth@lat-lon.de">Andreas Poth</a>
 */
public class Shape2GML {

	private double minx = 9E99;
	private double miny = 9E99;
	private double maxx = -9E99;
	private double maxy = -9E99;


	/** Creates a new instance of Shape2GML */
	public Shape2GML(String inFile, String outFile) throws IOException {

		FeatureCollection fc = null;
		try {
			fc = readShape( inFile );
		} catch(Exception e) {
			throw new IOException( "Couldn't read input file: \n" + e );
		}
		try {
			exportToGML( outFile, fc );
		} catch(Exception e) {
			throw new IOException( "Couldn't write ouput file: \n" + e );
		}
	}

	private FeatureCollection readShape(String inFile) throws Exception {

		System.out.println("reading " + inFile + " ... ");

		// open shape file
		ShapeFile sf = new ShapeFile( inFile );

		// get names of the columns of the dbase file
		String[] props = sf.getProperties();

		int count = sf.getRecordNum();

		FeatureFactory fac = new FeatureFactory();
		// create (empty feature collection)
		FeatureCollection fc = fac.createFeatureCollection( inFile, count );

		// load each feature from the shape file create a deegree feature
		// and add it to the feature collection
		for (int i = 0; i < count; i++) {

			if ( i % 10 == 0) {
				double v = (i*100)/count;
				System.out.print(".");
			}
			Feature feat = sf.getFeatureByRecNo( i+1 );
			GM_Object[] geom = feat.getGeometryProperties();
			if ( geom != null && geom.length > 0 && geom[0] != null ) {
				if ( !(geom[0] instanceof GM_Point) ) {
					GM_Envelope bbox = geom[0].getEnvelope();
					if ( bbox.getMin().getX() < minx ) minx = bbox.getMin().getX();
					if ( bbox.getMax().getX() > maxx ) maxx = bbox.getMax().getX();
					if ( bbox.getMin().getY() < miny ) miny = bbox.getMin().getY();
					if ( bbox.getMax().getY() > maxy ) maxy = bbox.getMax().getY();
				} else {
					GM_Point point = (GM_Point)geom[0];
					if ( point.getX() < minx ) minx = point.getX();
					if ( point.getX() > maxx ) maxx = point.getX();
					if ( point.getY() < miny ) miny = point.getY();
					if ( point.getY() > maxy ) maxy = point.getY();
				}
			}
			// add to feature collection
			fc.appendFeature( feat );

		}
		System.out.println();

		sf.close();

		return fc;
	}

	private void exportToGML(String outFile, FeatureCollection fc) throws Exception {
		System.out.println("writing " + outFile + " ... ");

		GMLDocument gmlDoc = new GMLDocument_Impl();
		PrintWriter pw = new PrintWriter( new FileOutputStream( outFile ) );
		pw.println( "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" );
		pw.println( "<collection xmlns:gml=\"http://www.opengis.net/gml\" >" );
		pw.println( "<gml:boundedBy><gml:box><gml:coord><gml:X>" + minx + "</gml:X>" );
		pw.println( "<gml:Y>" + miny + "</gml:Y></gml:coord>");
		pw.println( "<gml:coord><gml:X>" + maxx + "</gml:X>" );
		pw.println( "<gml:Y>" + maxy + "</gml:Y></gml:coord></gml:box></gml:boundedBy>" );

		for (int i = 0; i < fc.getSize(); i++) {

			if ( i % 10 == 0) {
				double v = (i*100)/fc.getSize();
				System.out.print(".");
			}

			Feature feat = fc.getFeature( i );
			FeatureType ft = feat.getFeatureType();
			FeatureTypeProperty[] ftp = ft.getProperties();

			GMLProperty[] gmlProp = new GMLProperty[ftp.length];
			int cnt = 0;
			for (int j = 0; j < ftp.length; j++) {
				Object o = feat.getProperty( ftp[j].getName() );
				if ( o instanceof GM_Object ) {
					GMLGeometry geo = GMLFactory.createGMLGeometry( (GM_Object)o );
					gmlProp[cnt++] = GMLGeoProperty_Impl.createGMLGeoProperty( geo );
				} else {
					if ( o != null ) {
						gmlProp[cnt++] = GMLProperty_Impl.createGMLProperty( gmlDoc.getDocument(),
									ftp[j].getName(),o.toString().trim() );
					} else {
						gmlProp[cnt++] = GMLProperty_Impl.createGMLProperty( gmlDoc.getDocument(),
									ftp[j].getName(),"" );
					}
				}
			}

			GMLFeature gmlFeat =
				GMLFeature_Impl.createGMLFeature( gmlDoc.getDocument(), "Shape", ""+i, gmlProp );

			pw.println( "<gml:featureMember>" );
			pw.println( gmlFeat.toString() );
			pw.println( "</gml:featureMember>" );
		}

		pw.println( "</collection>" );
		pw.close();

		System.out.println();
		System.out.println("finished!");
	}

	/**
	 * @param args the command line arguments
	 */
	/*public static void main(String[] args) {

		if ( args == null || args.length < 2 ) {
			System.out.println("Two arguments - input file and output file - are required!");
			System.exit(1);
		}

		try {
			new Shape2GML( args[0], args[1] );
		} catch(Exception e) {
			System.out.println(e);
		}

	}*/

}
