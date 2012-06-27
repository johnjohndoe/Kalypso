package org.kalypso.statistics.gui.views.nodes;

import org.kalypso.statistics.types.EColumnLabel;

public enum EColumnLabelsNodes implements EColumnLabel {
	SELECTION("Selection", 65), //
	NODE_TYPE("Type", 160), //
	NAME("Name", 250), //
	DESCRIPTION("Description", 350);

	private final String m_lbl;
	private final int m_width;

	private EColumnLabelsNodes(final String lbl, final int width) {
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