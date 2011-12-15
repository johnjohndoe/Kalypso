package org.kalypso.statistics.gui.views.nodes;

import java.util.List;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.statistics.types.data.NodeProfile;

/**
 * This class reads the data model and returns the data as an array.
 * 
 */
public class NodesContentProvider implements IStructuredContentProvider {

	@Override
	public Object[] getElements(final Object inputElement) {
		@SuppressWarnings("unchecked")
		final List<NodeProfile> customers = (List<NodeProfile>) inputElement;
		return customers.toArray(new NodeProfile[customers.size()]);
	}

	@Override
	public void dispose() {
	}

	@Override
	public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
		viewer.refresh();
	}

}
