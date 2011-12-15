package org.kalypso.statistics.test;

import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.Charset;
import java.text.SimpleDateFormat;

import org.deegree.io.dbaseapi.DBaseException;
import org.deegree.io.shpapi.shape_new.ShapeFile;
import org.deegree.io.shpapi.shape_new.ShapeFileReader;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.feature.FeatureException;
import org.deegree.model.feature.GMLFeatureAdapter;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.shape.FileMode;
import org.kalypso.shape.ShapeDataException;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypso.shape.dbf.IDBFValue;
import org.kalypso.shape.deegree.SHP2GM_Object;
import org.kalypso.shape.geometry.ISHPGeometry;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;

public class TestShp2GML {

	private String inFile = null;

	private String outFile = null;

	public TestShp2GML(String inFile, String outFile) {
		if (inFile.endsWith(".shp")) {
			inFile = inFile.substring(0, inFile.lastIndexOf("."));
		}
		this.inFile = inFile;
		this.outFile = outFile;
	}

	private FeatureCollection read() throws IOException, DBaseException {
		// open shape file
		ShapeFileReader reader = new ShapeFileReader(inFile);
		ShapeFile sf = reader.read();

		return sf.getFeatureCollection();
	}

	private void write(FeatureCollection fc) throws IOException, FeatureException {
		FileOutputStream fos = new FileOutputStream(outFile);
		new GMLFeatureAdapter().export(fc, fos);
		fos.close();
	}

	public static void main(String[] args) throws Exception {
		// TestShp2GML s2g = new TestShp2GML("D:\\_temp\\Knoten.shp",
		// "D:\\_temp\\out.gml");
		// s2g.write(s2g.read());

		org.kalypso.shape.ShapeFile shapeFile = new org.kalypso.shape.ShapeFile("D:\\_temp\\Knoten", Charset.defaultCharset(), FileMode.READ);
		final IDBFField[] fields = shapeFile.getFields();

		System.out.println(shapeFile.getShapeType().getLabel());
		for (int row = 0; row < shapeFile.getNumRecords(); row++) {
			final Object[] data = shapeFile.getRow(row);
			final ISHPGeometry shape = shapeFile.getShape(row);
			GM_Point node = toNode(shape);
			if (node != null) {
				for (int i = 0; i < fields.length; i++) {
					IDBFField field = fields[i];
					System.out.println(node.getX());
					System.out.println(String.format("%s: %s - %s", field.getName(), toString(field, data[i]), node.toString()));
				}
			}
		}
	}

	private static GM_Point toNode(final ISHPGeometry shape) throws GM_Exception, CoreException {
		final GM_Object gmObject = SHP2GM_Object.transform("EPSG:31467", shape);
		if (gmObject instanceof GM_Point) {
			return (GM_Point) gmObject;
		}
		return null;
	}

	private static String toString(IDBFField field, Object element) throws ShapeDataException {
		switch (field.getType()) {
		case F:
			return String.format("%.2f", element);
		case L:
			return ((Boolean) element).toString();
		case D:
			return new SimpleDateFormat("dd.mm.YYYY").format(element);
		case C:
		case M:
		case N:
		default:
			return element.toString();
		}
	}
}
