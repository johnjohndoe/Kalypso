package org.kalypso.statistics.types.data;

import java.util.Calendar;

public class TimeserieProfileEntry extends AbstractStatisticsRecordType {

	private final Calendar m_time;
	private double m_value;

	public TimeserieProfileEntry(int UID, Calendar time, double value) {
		super();
		m_time = time;
		m_value = value;
		setUID(UID);
	}

	/**
	 * @return the time
	 */
	public Calendar getTime() {
		return m_time;
	}

	/**
	 * @return the value
	 */
	public Double getValue() {
		return m_value;
	}

	public void setValue(double value) {
		m_value = value;
	}

}
