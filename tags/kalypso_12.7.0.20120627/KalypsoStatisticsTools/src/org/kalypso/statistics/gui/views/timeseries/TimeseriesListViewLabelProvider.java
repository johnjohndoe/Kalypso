package org.kalypso.statistics.gui.views.timeseries;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.Image;
import org.joda.time.Duration;
import org.kalypso.statistics.gui.views.AbstractListViewLabelProvider;
import org.kalypso.statistics.plugin.ImagesProvider;
import org.kalypso.statistics.types.EColumnLabel;
import org.kalypso.statistics.types.EStatisticsImage;
import org.kalypso.statistics.types.data.TimeserieProfile;
import org.kalypso.statistics.utils.AppUtils;

public class TimeseriesListViewLabelProvider extends AbstractListViewLabelProvider<TimeserieProfile> {
	// We use icons
	private static final Image UNKNOWN = ImagesProvider.getImage(EStatisticsImage.ICON_UNKNOWN_16);
	private static final Image WATERLEVEL = ImagesProvider.getImage(EStatisticsImage.ICON_WATERLEVEL_16);
	private static final Image RAINFALL = ImagesProvider.getImage(EStatisticsImage.ICON_RAINFALL_16);
	private static final Image RUNOFF = ImagesProvider.getImage(EStatisticsImage.ICON_RUNOFF_16);
	private static final Image EXTREMS = ImagesProvider.getImage(EStatisticsImage.ICON_EXTREMS_16);
	
	private final List<EColumnLabel> m_fields = new ArrayList<EColumnLabel>();

	public TimeseriesListViewLabelProvider(final List<EColumnLabel> fields) {
		for (final EColumnLabel fld : fields) {
			m_fields.add(fld);
		}
	}

	@Override
	protected EColumnLabelsTimeseries getEnumIndex(final int columnIndex) {
		return (EColumnLabelsTimeseries) m_fields.get(columnIndex);
	}

	@Override
	protected String setCellText(final TimeserieProfile element, final EColumnLabel columnIndex) {
		final EColumnLabelsTimeseries index = (EColumnLabelsTimeseries) columnIndex;
		switch (index) {
		case SELECTION:
			return "";
		case NAME:
			return element.getName();
		case DESCRIPTION:
			return element.getDescription();
		case PHENOMENA:
			return element.getTimeseriesType().getLabel();
		case NODE:
			final String nodeName = element.getNodeProfileName();
			if (nodeName == null || nodeName.trim().length() == 0) {
				return "<undefined>";
			}
			return nodeName;
		case TIME_FROM:
			// return Long.toString(element.getTimeFrom().getTimeInMillis());
			return AppUtils.DISPLAY_DATETIME_FORMAT.format(element.getTimeFrom().getTime());
		case TIME_TO:
			return AppUtils.DISPLAY_DATETIME_FORMAT.format(element.getTimeTo().getTime());
		case NUMBER_OF_ENTRIES:
			return Integer.toString(element.getNumberOfEntries());
		case TIMESTEP:
			Duration duration = new Duration(element.getTimestepMillis());
			if (duration.getStandardSeconds() <= 90)
				return String.format("%d sec", duration.getStandardSeconds());
			if (duration.getStandardMinutes() <= 90)
				return String.format("%d min", duration.getStandardMinutes());
			if (duration.getStandardHours() <= 24)
				return String.format("%d hours", duration.getStandardHours());
			else
				return String.format("%d days", duration.getStandardDays());
		default:
			throw new RuntimeException("Should not happen"); //$NON-NLS-1$
		}
	}

	@Override
	protected Image setCellImage(final TimeserieProfile element, final EColumnLabel columnIndex) {
		if (EColumnLabelsTimeseries.PHENOMENA.equals(columnIndex)) {
			switch (element.getTimeseriesType()) {
			case PRECIPITATION:
				return RAINFALL;
			case RUNOFF:
				return RUNOFF;
			case WATERLEVEL:
				return WATERLEVEL;
			case PRECIPITATION_EXTREMS:
			case WATERLEVEL_EXTREMS:
				return EXTREMS;
			default:
				return UNKNOWN;
			}
		}
		return null;
	}

}
