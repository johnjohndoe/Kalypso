package org.kalypsodeegree_impl.model.cv;

import java.io.File;
import java.util.Vector;

/**
 * Class which holds the rangeSetData of a RectifiedGridCoverage
 * 
 * @author N. Peiler
 *  
 */
public class RangeSet {

	/**
	 * Vector, which stores the rangeSet data; the data of each row is stored in
	 * a Vector
	 */
	private Vector rangeSetData = null;

	/**
	 * location, where rangeSetData is stored
	 */
	File rangeSetDataFile = null;

	/**
	 * constructs a RangeSet with the given rangeSetData
	 * 
	 * @param rangeSetData
	 * @param rangeSetDataFile
	 *            location where rangeSetData is stored
	 */
	public RangeSet(Vector rangeSetData, File rangeSetDataFile) {
		this.rangeSetData = rangeSetData;
		this.rangeSetDataFile = rangeSetDataFile;
	}

	/**
	 * @return Returns the rangeSetData.
	 */
	public Vector getRangeSetData() {
		return rangeSetData;
	}

	/**
	 * @param rangeSetData
	 *            The rangeSetData to set.
	 */
	public void setRangeSetData(Vector rangeSetData) {
		this.rangeSetData = rangeSetData;
	}

	/**
	 * @return Returns the rangeSetDataFile.
	 */
	public File getRangeSetDataFile() {
		return rangeSetDataFile;
	}

	/**
	 * @param rangeSetDataFile
	 *            The rangeSetDataFile to set.
	 */
	public void setRangeSetDataFile(File rangeSetDataFile) {
		this.rangeSetDataFile = rangeSetDataFile;
	}

	/**
	 * @return the minValue of the rangeSetData
	 */
	public double getMinValue() {
		double min = Double.MAX_VALUE;
		for (int i = 0; i < rangeSetData.size(); i++) {
			Vector rowData = (Vector) rangeSetData.get(i);
			for (int j = 0; j < rowData.size(); j++) {
				if (rowData.get(j) != null) {
					double actualValue = ((Double) rowData.get(j))
							.doubleValue();
					if (actualValue < min) {
						min = actualValue;
					}
				}
			}//for j
		}//for i
		return min;
	}

	/**
	 * @return the maxValue of the rangeSetData
	 */
	public double getMaxValue() {
		double max = Double.MIN_VALUE;
		for (int i = 0; i < rangeSetData.size(); i++) {
			Vector rowData = (Vector) rangeSetData.get(i);
			for (int j = 0; j < rowData.size(); j++) {
				if (rowData.get(j) != null) {
					double actualValue = ((Double) rowData.get(j))
							.doubleValue();
					if (actualValue > max) {
						max = actualValue;
					}
				}
			}//for j
		}//for i
		return max;
	}
}