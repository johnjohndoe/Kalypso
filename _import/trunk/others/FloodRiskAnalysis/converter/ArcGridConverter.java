package converter;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.TreeMap;
import java.util.Vector;

import org.deegree.model.coverage.GridRange;
import org.deegree.model.geometry.GM_Point;
import org.deegree_impl.model.cs.ConvenienceCSFactory;
import org.deegree_impl.model.cv.GridRange_Impl;
import org.deegree_impl.model.cv.RangeSet;
import org.deegree_impl.model.cv.RectifiedGridCoverage;
import org.deegree_impl.model.cv.RectifiedGridDomain;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

import tools.Number;
import view.LogView;

/**
 * 
 * Class for reading and writing ascii-Grid files
 * 
 * @author N. Peiler
 * 
 *  
 */
public class ArcGridConverter {

	public ArcGridConverter() {
	  super();
	}

	/**
	 * exports a RectifiedGridCoverage to an Ascii-Grid
	 * 
	 * @param out
	 *            ascii output file
	 * @param grid
	 *            RectifiedGridCoverage to export
	 * @throws Exception
	 */
	public void exportGridArc(File out, RectifiedGridCoverage grid)
			throws Exception {
		BufferedWriter bw = new BufferedWriter(new FileWriter(out));
		RectifiedGridDomain gridDomain = grid.getGridDomain();
		RangeSet rangeSet = grid.getRangeSet();
		int nCols = gridDomain.getNumColumns();
		int nRows = gridDomain.getNumRows();
		GM_Point origin = gridDomain.getOrigin(null);
		double originX = origin.getX();
		double originY = origin.getY();
		double offsetX = gridDomain.getOffsetX(origin.getCoordinateSystem());
		int nodata = -9999;
		Vector rangeSetData = rangeSet.getRangeSetData();
		try {
			bw.write("ncols " + nCols + "\nnrows " + nRows + "\nxllcorner "
					+ originX + "\nyllcorner " + originY + "\ncellsize "
					+ offsetX + "\nnodata_value " + nodata);
			bw.newLine();

			for (int i = 0; i < rangeSetData.size(); i++) {
				Vector rowData = (Vector) rangeSetData.get(i);
				for (int j = 0; j < rowData.size(); j++) {
					if (rowData.get(j) != null) {
						double value = ((Double) rowData.get(j)).doubleValue();
						double roundValue = Number.round(value, 6,
								BigDecimal.ROUND_HALF_EVEN);
						bw.write(roundValue + " ");
					} else {
						bw.write(nodata + " ");
					}
					/*
					 * System.out.println(i + "," + j + ": " + rowData.get(j) + "
					 * ");
					 */
				}//for j
				bw.newLine();
			}//for i
			bw.close();
		}// try
		catch (IOException e) {
			bw.write("Error while writing ascii-arc file: " + e.getMessage());
			throw e;
		}// catch
		finally {
			bw.close();
			if (out.length() == 0)
				out.delete();
		}// finally
	}//exportGridArc

	/**
	 * imports an ascii-grid file
	 * 
	 * @param in
	 *            input file
	 * @return RectifiedGridCoverage
	 */
	public RectifiedGridCoverage importGridArc(File in) {
		int nCols = 0;
		int nRows = 0;
		GM_Point origin = null;
		//Gauß-Krüger
		CS_CoordinateSystem srs = ConvenienceCSFactory.getInstance()
				.getOGCCSByName("EPSG:31467");
		double[] offset = new double[2];
		String nodata = null;
		Vector rangeSetData = new Vector();
		//StringBuffer rangeData = new StringBuffer();
		Vector rangeData = new Vector();
		try {
			BufferedReader br = new BufferedReader(new FileReader(in));
			String[] data = new String[6];
			String line;
			for (int i = 0; i < data.length; i++) {
				line = br.readLine();
				int index = line.indexOf(" ");
				String subString = line.substring(index);
				data[i] = subString.trim();
				System.out.println(data[i]);
			}
			nCols = new Integer(data[0]).intValue();
			nRows = new Integer(data[1]).intValue();
			double originX = new Double(data[2]).doubleValue();
			double originY = new Double(data[3]).doubleValue();
			//double originZ = 0;
			origin = GeometryFactory.createGM_Point(originX, originY, srs);
			offset[0] = (new Double(data[4])).doubleValue();
			offset[1] = (new Double(data[4])).doubleValue();
			//offset[2] = 0;
			nodata = data[5];
			while ((line = br.readLine()) != null) {
				//rangeData.append(line);
				String[] dataAsString = line.split(" ");
				//System.out.println("...");
				for (int i = 0; i < dataAsString.length; i++) {
					rangeData.addElement(dataAsString[i]);
				}
			}
		} catch (Exception e) {
			System.out.println(e);
		}
		double[] low = { 0.0, 0.0 };
		double[] high = { nCols, nRows };
		GridRange gridRange = new GridRange_Impl(low, high);
		RectifiedGridDomain gridDomain = new RectifiedGridDomain(origin,
				offset, gridRange);
		//String[] dataAsString = rangeData.toString().split(" ");
		for (int i = 0; i < nRows; i++) {
			Vector rowData = new Vector();
			for (int n = 0; n < nCols; n++) {
				if (rangeData.get(n + (i * nCols)).equals(nodata)) {
					rowData.addElement(null);
				} else {
					double actualValue = Double.parseDouble((String) rangeData
							.get(n + (i * nCols)));
					rowData.addElement(new Double(actualValue));
				}
			}
			//System.out.println(rowData);
			//System.out.println(i+" of "+nRows+"calculated");
			rangeSetData.addElement(rowData);
		}
		RangeSet rangeSet = new RangeSet(rangeSetData, null);
		RectifiedGridCoverage grid = new RectifiedGridCoverage(gridDomain,
				rangeSet);
		return grid;
	}

	/**
	 * imports waterlevelAsciiGrids (FileName: "wsp_hqxx.asc")
	 * 
	 * @param waterlevelDirectory
	 *            Directory with Waterlevel-asciiFiles
	 * @return TreeMap of WaterlevelGrids (key=annuality,
	 *         value=RectifiedGridCoverage)
	 */
	public TreeMap importwaterlevelGrids(File waterlevelDirectory) {
		TreeMap waterlevelGrids = new TreeMap();
		File[] waterlevelFiles = waterlevelDirectory.listFiles();
		for (int i = 0; i < waterlevelFiles.length; i++) {
			if (waterlevelFiles[i].getName().startsWith("wsp_")) {
				File waterlevelFile = waterlevelFiles[i];
				LogView.println(waterlevelFile.getName());
				RectifiedGridCoverage waterlevelGrid = importGridArc(waterlevelFile);
				String fileName = waterlevelFile.getName();
				System.out.println(fileName);
				int index1 = fileName.indexOf("q");
				int index2 = fileName.indexOf(".");
				String temp = fileName.substring(index1 + 1, index2);
				System.out.println(temp);
				Double years = new Double(temp);
				double annuality = 1 / years.doubleValue();
				waterlevelGrids.put(new Double(annuality), waterlevelGrid);
			}
		}
		return waterlevelGrids;
	}
}