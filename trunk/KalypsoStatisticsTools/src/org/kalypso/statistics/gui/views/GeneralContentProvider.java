package org.kalypso.statistics.gui.views;

import java.util.List;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

/**
 * This class reads the data model and returns the data as an array.
 * 
 */
public class GeneralContentProvider<T> implements IStructuredContentProvider {

	@Override
	public Object[] getElements(final Object inputElement) {
		@SuppressWarnings("unchecked")
		final List<T> elements = (List<T>) inputElement;
		return elements.toArray();
	}

	@Override
	public void dispose() {
	}

	@Override
	public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
	}

}
