package org.kalypso.statistics.types;

import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;

public enum ETimeseriesType implements ILabel {
	UNKNOWN("<unknown>", "Other (unknown)", ""), //
	WATERLEVEL(ITimeseriesConstants.TYPE_WATERLEVEL, "Waterlevel", "m"), //
	PRECIPITATION(ITimeseriesConstants.TYPE_RAINFALL, "Precipitation", "mm"), //
	RUNOFF(ITimeseriesConstants.TYPE_RUNOFF, "Discharge", "m³/s"), //
	//
	WATERLEVEL_EXTREMS(ITimeseriesConstants.TYPE_WATERLEVEL, "Waterlevel Extrems", "m"), //
	PRECIPITATION_EXTREMS(ITimeseriesConstants.TYPE_RAINFALL, "Precipitation Extrems", "mm"), //
	
	;

	private final String m_abbreviation;
	private final String m_label;
	private final String m_unit;

	ETimeseriesType(String abbreviation, String label, String unit) {
		m_abbreviation = abbreviation;
		m_label = label;
		m_unit = unit;
	}

	/**
	 * @return the abbreviation
	 */
	public String getAbbreviation() {
		return m_abbreviation;
	}

	/**
	 * @return the label
	 */
	@Override
	public String getLabel() {
		return m_label;
	}

	public static ETimeseriesType getTypeFor(final String abbreviation) {
		for (ETimeseriesType type : ETimeseriesType.values()) {
			if (type.getAbbreviation().equals(abbreviation)) {
				return type;
			}
		}
		return ETimeseriesType.UNKNOWN;
	}

	/**
	 * @return the unit
	 */
	public String getUnit() {
		return m_unit;
	}
}
