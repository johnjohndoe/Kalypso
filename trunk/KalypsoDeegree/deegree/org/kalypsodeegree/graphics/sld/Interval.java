package org.kalypsodeegree.graphics.sld;

/**
 * @author N. Peiler
 *
 */
public interface Interval {
	
	double getLowerLimit();
	
	void setLowerLimit(double lowerLimit);
	
	double getUpperLimit();
	
	void setUpperLimit(double upperLimit);
	
	boolean contains(double x);
}
