package org.kalypso.statistics.gui.wizards.importNodesFromSHP;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.gml.ui.commands.importshape.ImportShapeWizardPage;
import org.kalypso.statistics.gui.SimpleLabelProvider;
import org.kalypso.statistics.types.ENodeProfileType;

public class NodeShapeImportWizardPage extends ImportShapeWizardPage {

	private ComboViewer m_importTypeSelectionViewer;

	public NodeShapeImportWizardPage(String pageName, String[] properties) {
		super(pageName, properties);
	}

	@Override
	public void addCustomControls(Composite container) {
		final Composite group = new Group(container, SWT.NONE);
		group.setLayout(new GridLayout(3, false));
		group.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		final Label label = new Label(group, SWT.NONE);
		label.setText("Import data type");
		m_importTypeSelectionViewer = new ComboViewer(group, SWT.BORDER | SWT.READ_ONLY);
		m_importTypeSelectionViewer.setContentProvider(new ArrayContentProvider());
		m_importTypeSelectionViewer.setLabelProvider(new SimpleLabelProvider());
		m_importTypeSelectionViewer.setSorter(new ViewerSorter());
		m_importTypeSelectionViewer.setInput(ENodeProfileType.values());
		m_importTypeSelectionViewer.setSelection(new StructuredSelection(ENodeProfileType.values()[0]));
		final Control control = m_importTypeSelectionViewer.getControl();
		control.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));
		m_importTypeSelectionViewer.getControl().setEnabled(true);
	}

	public ENodeProfileType getImportSelectionType() {
		if (m_importTypeSelectionViewer == null || m_importTypeSelectionViewer.getControl().isDisposed())
			return null;
		final ISelection selection = m_importTypeSelectionViewer.getSelection();
		if (!selection.isEmpty() && selection instanceof IStructuredSelection) {
			final IStructuredSelection structuredSelection = (IStructuredSelection) selection;
			final Object selectedElement = structuredSelection.getFirstElement();
			if (selectedElement instanceof ENodeProfileType) {
				return (ENodeProfileType) selectedElement;
			}
		}
		return null;

	}

}
