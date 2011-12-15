package org.kalypso.statistics.gui;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.kalypso.statistics.types.EColumnLabel;

public abstract class AbstractListViewSorter extends ViewerSorter {
	public static enum ORDER {
		ASCENDING, DESCENDING
	}

	private EColumnLabel m_propertyIndex;
	private ORDER m_direction;
	private final ORDER m_defaultDirection;

	public AbstractListViewSorter(final EColumnLabel defaultPropertyIndex) {
		this(defaultPropertyIndex, ORDER.ASCENDING);
	}

	public AbstractListViewSorter(final EColumnLabel defaultPropertyIndex, final ORDER order) {
		setPropertyIndex(defaultPropertyIndex);
		m_defaultDirection = order;
		m_direction = m_defaultDirection;
	}

	public final void setColumn(final EColumnLabel columnIndex) {
		if (columnIndex.equals(getPropertyIndex())) {
			// Same column as last sort; toggle the direction
			m_direction = m_direction.equals(ORDER.DESCENDING) ? ORDER.ASCENDING : ORDER.DESCENDING;
		} else {
			// New column; do sorting
			setPropertyIndex(columnIndex);
			m_direction = m_defaultDirection;
		}
	}

	private void setPropertyIndex(final EColumnLabel propertyIndex) {
		m_propertyIndex = propertyIndex;
	}

	public EColumnLabel getPropertyIndex() {
		return m_propertyIndex;
	}

	@Override
	public final int compare(final Viewer viewer, final Object e1, final Object e2) {
		final int order = doCompare(e1, e2);
		// If descending order, flip the direction
		return m_direction.equals(ORDER.DESCENDING) ? -order : order;
	}

	public abstract int doCompare(final Object e1, final Object e2);

}
