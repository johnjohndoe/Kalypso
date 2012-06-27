package org.kalypso.statistics.types;

public class CommandToolbarItem {
	private final String m_label;
	private final String m_tooltip;

	public CommandToolbarItem(final String label, final String tooltip) {
		m_label = label;
		m_tooltip = tooltip;
	}

	public String getLabel() {
		return m_label;
	}

	public String getTooltip() {
		return m_tooltip;
	}

}
