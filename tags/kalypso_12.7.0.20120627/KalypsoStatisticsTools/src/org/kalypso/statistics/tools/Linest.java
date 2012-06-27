package org.kalypso.statistics.tools;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import org.apache.commons.math.stat.regression.SimpleRegression;

/**
 * Fits a least mean squares straight line to a vector of Y values and a range
 * of X values. Returns a block of coefficient values, an intercept value, and
 * regression statistics in a certain order.
 * 
 * @author antanas
 * 
 */
public class Linest {

	private static final DateFormat DATE_FORMAT = new SimpleDateFormat(
			"dd.MM.yyyy");

	public static void main(String[] args) throws ParseException {
		final String[] dates = new String[] { "03.01.2040", "14.02.2040",
				"10.08.2040", "19.09.2040", "18.12.2040", "25.01.2041",
				"18.04.2041", "23.07.2041", "26.09.2041", "03.11.2041",
				"02.03.2042", "07.05.2042", "07.06.2042", "30.06.2042" };
		final double[] values = new double[] { 12.9850, 10.3750, 7.3390,
				23.3070, 8.6350, 7.9400, 7.1470, 5.6960, 8.0730, 18.2970,
				14.2810, 4.6620, 6.4820, 14.0140 };
		SimpleRegression regression = new SimpleRegression();
		for (int i = 0; i < dates.length; i++) {
			regression
					.addData(DATE_FORMAT.parse(dates[i]).getTime(), values[i]);
		}
		for (int i = 0; i < dates.length; i++) {
			System.out.println(regression.predict(DATE_FORMAT.parse(dates[i])
					.getTime()));
		}
	}
}
