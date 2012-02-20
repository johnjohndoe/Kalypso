package org.kalypso.statistics.gui;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.kalypso.statistics.types.data.NodeProfile;

public class NodeLabelProvider extends ColumnLabelProvider {

	@Override
	public String getText(Object element) {
		if (element instanceof NodeProfile) {
			return ((NodeProfile) element).getName();
		}
		return element.toString();
	}
}
