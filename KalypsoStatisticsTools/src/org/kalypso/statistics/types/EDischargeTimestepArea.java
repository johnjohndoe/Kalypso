package org.kalypso.statistics.types;

import java.util.ArrayList;
import java.util.List;

public enum EDischargeTimestepArea {
	AREA_MINUTES("Minute area", 60 * 1000, new int[] { 1, 2, 3, 4, 6, 9, 12, 18 }), //
	AREA_HOURS("Hours area", 60 * 60 * 1000, new int[] { 2, 3, 4, 6, 9, 12, 18 }), //
	AREA_DAYS("Days area", 24 * 60 * 60 * 1000, new int[] { 1, 2, 3, 4, 6 });

	private final String m_label;
	private final List<Integer> m_multiplicators;
	private final long m_timestepMillis;

	EDischargeTimestepArea(String label, long timestepMillis, int[] multiplicators) {
		m_label = label;
		m_timestepMillis = timestepMillis;
		m_multiplicators = new ArrayList<Integer>();
		for (int i = 0; i < multiplicators.length; i++) {
			getMultiplicators().add(multiplicators[i]);
		}
	}

	/**
	 * @return the label
	 */
	public String getLabel() {
		return m_label;
	}

	/**
	 * @return the multiplicators
	 */
	public List<Integer> getMultiplicators() {
		return m_multiplicators;
	}

	/**
	 * @return the timestepMillis
	 */
	public long getTimestepMillis() {
		return m_timestepMillis;
	}
}
