package org.kalypso.statistics.gui.views.timeseries;

import org.kalypso.statistics.gui.AbstractListViewSorter;
import org.kalypso.statistics.types.data.TimeserieProfile;

public class TimeseriesListViewSorter extends AbstractListViewSorter {

	public TimeseriesListViewSorter(final EColumnLabelsTimeseries defaultPropertyIndex) {
		super(defaultPropertyIndex);
	}

	@Override
	public int doCompare(final Object e1, final Object e2) {
		final TimeserieProfile p1 = (TimeserieProfile) e1;
		final TimeserieProfile p2 = (TimeserieProfile) e2;
		final EColumnLabelsTimeseries index = (EColumnLabelsTimeseries) getPropertyIndex();
		switch (index) {
		case NAME:
			return p1.getName().compareToIgnoreCase(p2.getName());
		case SELECTION:
			return 0;
		case NUMBER_OF_ENTRIES:
			return new Integer(p1.getNumberOfEntries()).compareTo(p2.getNumberOfEntries());
		case TIMESTEP:
			return new Long(p1.getTimestepMillis()).compareTo(p2.getTimestepMillis());
		case TIME_FROM:
			return p1.getTimeFrom().compareTo(p2.getTimeFrom());
		case TIME_TO:
			return p1.getTimeTo().compareTo(p2.getTimeTo());
		case DESCRIPTION:
			return p1.getDescription().compareTo(p2.getDescription());
		case NODE:
			return p1.getNodeProfileName().compareTo(p2.getNodeProfileName());
		case PHENOMENA:
			return p1.getTimeseriesType().getLabel().compareTo(p2.getTimeseriesType().getLabel());
		default:
			return 0;
		}
	}

}
