package org.kalypso.statistics.types;

public enum ENodeProfileType implements ILabel {

	HYDROLOGICAL_NODE("Hydological node"), //
	STATION("Gauging station"), //
	UNKNOWN("Unknown"), //
	;

	private final String m_label;

	ENodeProfileType(String label) {
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
