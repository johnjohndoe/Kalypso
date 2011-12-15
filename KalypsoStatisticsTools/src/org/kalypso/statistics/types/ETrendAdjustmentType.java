package org.kalypso.statistics.types;

public enum ETrendAdjustmentType implements ILabel {
	TIMESERIE_BEGINNING("Beginning of the time-serie"), //
	TIMESERIE_CENTER("Center of the time-serie"), //
	TIMESERIE_END("End of the time-serie");

	private final String m_label;

	ETrendAdjustmentType(String label) {
		m_label = label;
	}

	/**
	 * @return the label
	 */
	@Override
	public String getLabel() {
		return m_label;
	}
}
