package org.kalypso.statistics.gui.views.nodes;

import org.kalypso.statistics.gui.AbstractListViewSorter;
import org.kalypso.statistics.types.data.NodeProfile;

public class NodesListViewSorter extends AbstractListViewSorter {

	public NodesListViewSorter(final EColumnLabelsNodes defaultPropertyIndex) {
		super(defaultPropertyIndex);
	}

	@Override
	public int doCompare(final Object e1, final Object e2) {
		final NodeProfile p1 = (NodeProfile) e1;
		final NodeProfile p2 = (NodeProfile) e2;
		final EColumnLabelsNodes index = (EColumnLabelsNodes) getPropertyIndex();
		switch (index) {
		case SELECTION:
			return 0;
		case NAME:
			return p1.getName().compareToIgnoreCase(p2.getName());
		case DESCRIPTION:
			return p1.getDescription().compareToIgnoreCase(p2.getDescription());
		case NODE_TYPE:
			return p1.getNodeProfileType().compareTo(p2.getNodeProfileType());
		default:
			return 0;
		}
	}

}
