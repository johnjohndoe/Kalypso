package org.kalypso.statistics.types;

import java.util.Calendar;

public enum EHydrologicalInterval implements ILabel {
	CALENDAR_YEAR("Calendar year", "Calendar year", Calendar.JANUARY, 1, 0, 0, Calendar.DECEMBER, 31, 23, 59, true, false), //
	HYDROLOGIC_YEAR_NH("Hydrologic Year", "Hydrologic Year (north hemisphere): 01.11 - 31.10", Calendar.NOVEMBER, 1, 0, 0, Calendar.OCTOBER, 31, 23, 59, false,
			false), //
	HYDROLOGIC_WINTER_NH("Hydrologic Winter", "Hydrologic Winter (north hemisphere): 01.11 - 30.04", Calendar.NOVEMBER, 1, 0, 0, Calendar.APRIL, 30, 23, 59,
			false, true), //
	HYDROLOGIC_SUMMER_NH("Hydrologic Summer", "Hydrologic Summer (north hemisphere): 01.05 - 31.10", Calendar.MAY, 1, 0, 0, Calendar.OCTOBER, 31, 23, 59, true,
			true); //
	// HYDROLOGIC_YEAR_SH("Hydrologic Year (south hemisphere)", Calendar.JULY,
	// 1, 0, 0, Calendar.JUNE, 30, 23, 59, false), //
	// HYDROLOGIC_SUMMER_SH("Hydrologic Summer (south hemisphere)",
	// Calendar.JANUARY, 1, 0, 0, Calendar.JUNE, 30, 23, 59, true), //
	// HYDROLOGIC_WINTER_SH("Hydrologic Winter (south hemisphere)",
	// Calendar.JULY, 1, 0, 0, Calendar.DECEMBER, 31, 23, 59, true); //

	private final Calendar m_intervalBegin;
	private final Calendar m_intervalEnd;
	private final String m_label;
	private final boolean m_sameYearPeriod;
	private final boolean m_partialIntervalType;
	private final String m_shortLabel;

	EHydrologicalInterval(String shortLabel, String label, int beginMonth, int beginDay, int beginHour, int beginMin, int endMonth, int endDay, int endHour,
			int endMin, boolean sameYearPeriod, boolean partialIntervalType) {
		m_shortLabel = shortLabel;
		m_label = label;
		m_sameYearPeriod = sameYearPeriod;
		m_partialIntervalType = partialIntervalType;
		m_intervalBegin = Calendar.getInstance();
		m_intervalBegin.set(1389, beginMonth, beginDay, beginHour, beginMin);
		m_intervalEnd = Calendar.getInstance();
		m_intervalEnd.set(1389, endMonth, endDay, endHour, endMin);
	}

	/**
	 * @return the yearBegin
	 */
	public Calendar getintervalBegin() {
		return m_intervalBegin;
	}

	/**
	 * @return the yearEnd
	 */
	public Calendar getintervalEnd() {
		return m_intervalEnd;
	}

	/**
	 * @return the sameYearPeriod
	 */
	public boolean isSameYearPeriod() {
		return m_sameYearPeriod;
	}

	/**
	 * @return the label
	 */
	@Override
	public String getLabel() {
		return m_label;
	}

	public String getShortLabel() {
		return m_shortLabel;
	}

	/**
	 * @return the formatted label
	 */
	public String getLabelFormatted() {
		return String.format("%1$s (%2$td.%2$tm - %3$td.%3$tm)", getLabel(), getintervalBegin(), getintervalEnd());
	}

	/**
	 * @return the partialIntervalType
	 */
	public boolean isPartialIntervalType() {
		return m_partialIntervalType;
	}
}
