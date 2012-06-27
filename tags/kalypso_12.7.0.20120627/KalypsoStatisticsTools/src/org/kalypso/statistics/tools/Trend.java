package org.kalypso.statistics.tools;

import java.util.List;

import org.apache.commons.math.stat.regression.SimpleRegression;
import org.kalypso.statistics.types.ETrendAdjustmentType;
import org.kalypso.statistics.types.data.TimeserieProfile;
import org.kalypso.statistics.types.data.TimeserieProfileEntry;

public class Trend {

	public static TimeserieProfile doTrendAdjustment(final TimeserieProfile profile, ETrendAdjustmentType trendAdjustmentType) {
		final List<TimeserieProfileEntry> profileEntries = profile.getEntries();
		if (profileEntries.size() == 0)
			return profile;
		final TimeserieProfile trend = getTrend(profile);
		final List<TimeserieProfileEntry> trendEntries = trend.getEntries();
		final int index;
		switch (trendAdjustmentType) {
		case TIMESERIE_BEGINNING:
			index = 0;
			break;
		case TIMESERIE_CENTER:
			index = trendEntries.size() / 2;
			break;
		case TIMESERIE_END:
		default:
			index = trendEntries.size() - 1;
			break;
		}
		final double correctorVal = trendEntries.get(index).getValue();
		for (int i = 0; i < profileEntries.size(); i++) {
			double val = correctorVal - trendEntries.get(i).getValue() + profileEntries.get(i).getValue();
			trendEntries.get(i).setValue(val);
		}
		return trend;
	}

	/**
	 * Implements Excel TREND function.
	 * 
	 * Fits a linear least mean squares line to a given range of X and Y values, and
	 * returns a range of predicted new Y values for an input range of new X values
	 * based on the fitted line.
	 * 
	 * @author antanas
	 * 
	 */
	public static TimeserieProfile getTrend(final TimeserieProfile profile) {
		final SimpleRegression regression = new SimpleRegression();
		for (TimeserieProfileEntry e : profile.getEntries()) {
			regression.addData(e.getTime().getTimeInMillis(), e.getValue());
		}
		TimeserieProfile outProfile = TimeserieProfile.clone(profile, false, false);
		for (TimeserieProfileEntry e : profile.getEntries()) {
			double value = regression.predict(e.getTime().getTimeInMillis());
			outProfile.getEntries().add(new TimeserieProfileEntry(0, e.getTime(), value));
		}
		return outProfile;
	}

	public static double[] getParams(final TimeserieProfile profile) {
		final SimpleRegression regression = new SimpleRegression();
		for (TimeserieProfileEntry e : profile.getEntries()) {
			regression.addData(e.getTime().getTimeInMillis(), e.getValue());
		}
		double[] res = new double[3];
		res[0] = regression.getSlope();
		res[1] = regression.getIntercept();
		res[2] = regression.getR();
		return res;
	}

}
