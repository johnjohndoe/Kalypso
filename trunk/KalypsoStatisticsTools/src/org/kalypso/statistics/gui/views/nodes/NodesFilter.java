package org.kalypso.statistics.gui.views.nodes;

import org.kalypso.statistics.gui.AbstractViewFilter;
import org.kalypso.statistics.types.data.NodeProfile;

public class NodesFilter extends AbstractViewFilter {

	@Override
	public boolean match(final Object element, final String searchPattern) {
		final NodeProfile p = (NodeProfile) element;
		if (p.getName().matches(searchPattern)) {
			return true;
		}
		if (p.getNodeProfileType().getLabel().matches(searchPattern)) {
			return true;
		}
		if (p.getDescription().matches(searchPattern)) {
			return true;
		}
		return false;
	}
}
