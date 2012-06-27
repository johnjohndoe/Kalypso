package org.kalypso.statistics.gui.views.timeseries;

import org.kalypso.statistics.gui.AbstractViewFilter;
import org.kalypso.statistics.types.data.TimeserieProfile;
import org.kalypso.statistics.utils.AppUtils;

public class TimeseriesViewFilter extends AbstractViewFilter {

	@Override
	public boolean match(final Object element, final String searchPattern) {
		final TimeserieProfile p = (TimeserieProfile) element;
		if (p.getName().matches(searchPattern)) {
			return true;
		}
		if (p.getNodeProfileName().matches(searchPattern)) {
			return true;
		}
		if (p.getTimeseriesType().getLabel().matches(searchPattern)) {
			return true;
		}
		if(new Integer(p.getNumberOfEntries()).toString().matches(searchPattern)) {
			return true;
		}
		if (AppUtils.DISPLAY_DATETIME_FORMAT.format(p.getTimeFrom().getTime()).matches(searchPattern)) {
			return true;
		}
		if (AppUtils.DISPLAY_DATETIME_FORMAT.format(p.getTimeTo().getTime()).matches(searchPattern)) {
			return true;
		}
		if (p.getDescription().matches(searchPattern)) {
			return true;
		}
		return false;
	}
}
