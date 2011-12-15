package org.kalypso.statistics.gui.views.timeseries;

import org.kalypso.statistics.types.EColumnLabel;

public enum EColumnLabelsTimeseries implements EColumnLabel {
	SELECTION("Select", 65), //
	NODE("Node / Station", 180), //
	PHENOMENA("Data type", 150), //
	NAME("Name", 200), //
	TIME_FROM("Time from", 120), //
	TIME_TO("Time to", 120), //
	TIMESTEP("Timestep", 70), //
	NUMBER_OF_ENTRIES("Nr. of entries", 80), //
	DESCRIPTION("Description", 300), //
	;

	private final String m_lbl;
	private final int m_width;

	private EColumnLabelsTimeseries(final String lbl, final int width) {
		m_lbl = lbl;
		m_width = width;
	}

	@Override
	public String getLabel() {
		return m_lbl;
	}

	@Override
	public int getWidth() {
		return m_width;
	}
}