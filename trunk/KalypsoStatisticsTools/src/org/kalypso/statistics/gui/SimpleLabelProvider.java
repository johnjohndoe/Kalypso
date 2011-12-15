package org.kalypso.statistics.gui;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.kalypso.statistics.types.ILabel;

public class SimpleLabelProvider extends ColumnLabelProvider {

	@Override
	public String getText(Object element) {
		if (element instanceof ILabel) {
			ILabel label = (ILabel) element;
			return label.getLabel();
		}
		return element.toString();
	}
}
