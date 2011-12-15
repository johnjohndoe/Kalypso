package org.kalypso.statistics.tools;

import java.io.FileNotFoundException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.math.FunctionEvaluationException;
import org.apache.commons.math.MathException;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.interpolation.SplineInterpolator;
import org.apache.commons.math.analysis.interpolation.UnivariateRealInterpolator;
import org.apache.commons.math.stat.descriptive.SummaryStatistics;
import org.kalypso.statistics.types.data.TimeserieProfile;
import org.kalypso.statistics.types.data.TimeserieProfileEntry;
import org.kalypso.statistics.utils.CSVReader;

/**
 * This test detects outliers from normal distributions. The tested data are the
 * minimum and maximum values. The result is a probality that indicates that the
 * data belongs to the core population.
 * 
 * WARNING: If the investigated sample has some other, especially assymmetric
 * distribution (e.g. lognormal) then these tests give false results!
 * 
 * The test is based on the difference of the mean of the sample and the most
 * extreme data considering the standard deviation (Grubbs, 1950, 1969; DIN
 * 32645; DIN 38402).
 * 
 * @author antanas
 * 
 */
public class GrubbsTest {

	public static void main(final String[] args) {
		final double[] vals = new double[] { 0.01, 0.1, 0.15, 0.12, 3.0, -0.2, 0.04 };
		System.out.println(getGrubbsMax(vals, 0.01));
	}

	public static double getGrubbsMax(final TimeserieProfile profile, final double significance) {
		double[] values = new double[profile.getEntries().size()];
		int i = 0;
		for (TimeserieProfileEntry e : profile.getEntries()) {
			values[i++] = e.getValue();
		}
		return getGrubbsMax(values, significance);
	}

	/**
	 * Calculates the MAX outlier out of provided values, using Grubbs method. <br>
	 * (WARNING: test is not performed for MIN outlier!)
	 * 
	 * @param values
	 *            the list of values to be tested for outlier
	 * @param significance
	 *            the level of Grubbs test significance
	 * @return the max outlier value, or <code>Double.NaN</code> if the max
	 *         value is not an outlier, or of provided significance level is not
	 *         available or exception occurred
	 */
	public static double getGrubbsMax(final double[] values, final double significance) {
		if (values.length < 3)
			return Double.NaN;
		final SummaryStatistics statistics = new SummaryStatistics();
		for (int i = 0; i < values.length; i++) {
			statistics.addValue(values[i]);
		}
		final double standardDeviation = statistics.getStandardDeviation();
		final double mean = statistics.getMean();
		final double max = statistics.getMax();

		double grubbs = (max - mean) / standardDeviation;

		// fix the value with the Grubbs correction value
		final UnivariateRealFunction function = GRUBBS_INTERPOLATION.get(significance);
		if (function == null)
			return Double.NaN;
		try {
			grubbs -= function.value(values.length);
		} catch (final FunctionEvaluationException e) {
			e.printStackTrace();
			return Double.NaN;
		}
		return grubbs > 0 ? max : Double.NaN;
	}

	/**
	 * GRUBBS_REGRESSION map contains simple regressions based on Grubbs table,
	 * for different significance factors
	 */
	private static final Map<Double, UnivariateRealFunction> GRUBBS_INTERPOLATION = new HashMap<Double, UnivariateRealFunction>();

	static {

		// UnivariateRealInterpolator interpolator = new SplineInterpolator();
		// interpolator.interpolate(xval[], yval[]);

		// here we assume that the "grubbsValues.csv" file exists and has
		// correct syntax
		try {
			final InputStream stream = GrubbsTest.class.getResourceAsStream("./grubbsValues.csv");
			final CSVReader reader = new CSVReader(stream, '\t', Charset.defaultCharset());
			final String[][] strings = reader.readFile();
			final List<Double> significanceList = new ArrayList<Double>();
			final double[] nList = new double[strings.length - 1];
			final Map<Double, double[]> grubbsValuesMap = new HashMap<Double, double[]>();
			final int width = strings[0].length;
			for (int i = 1; i < width; i++) {
				final double alpha = Double.parseDouble(strings[0][i]);
				significanceList.add(alpha);
				grubbsValuesMap.put(alpha, new double[nList.length]);
			}
			for (int i = 1; i < strings.length; i++) {
				nList[i - 1] = Integer.parseInt(strings[i][0]);
				for (int j = 1; j < width; j++) {
					grubbsValuesMap.get(significanceList.get(j - 1))[i - 1] = Double.parseDouble(strings[i][j]);
				}
			}
			for (final Double alpha : significanceList) {
				final UnivariateRealInterpolator interpolator = new SplineInterpolator();
				final UnivariateRealFunction function = interpolator.interpolate(nList, grubbsValuesMap.get(alpha));
				GRUBBS_INTERPOLATION.put(alpha, function);
			}
		} catch (final FileNotFoundException e) {
			e.printStackTrace();
		} catch (final MathException e) {
			e.printStackTrace();
		}
	}

}
