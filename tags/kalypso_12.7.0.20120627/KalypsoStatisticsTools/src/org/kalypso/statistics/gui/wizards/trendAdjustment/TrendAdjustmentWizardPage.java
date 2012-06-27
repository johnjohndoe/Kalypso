package org.kalypso.statistics.gui.wizards.trendAdjustment;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.statistics.gui.SimpleLabelProvider;
import org.kalypso.statistics.types.ETrendAdjustmentType;

public class TrendAdjustmentWizardPage extends WizardPage {

	private ComboViewer m_typeSelectionViewer;
	private Button m_copyButton;

	public TrendAdjustmentWizardPage(final String pageName) {
		super(pageName, "", null);
		setTitle("Trend adjustment");
		setDescription("Trend adjustment for the selected timeseries");
	}

	@Override
	public void createControl(final Composite parent) {
		initializeDialogUnits(parent);
		final Composite topLevel = new Composite(parent, SWT.NONE);
		topLevel.setLayout(new GridLayout());
		setControl(topLevel);
		createControlSource(topLevel);
	}

	public void createControlSource(final Composite parent) {
		final Group group = new Group(parent, SWT.NONE);
		group.setLayout(new GridLayout(2, false));
		group.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
		group.setText("Select trend adjustment type");

		new Label(group, SWT.NONE).setText("Type");
		m_typeSelectionViewer = new ComboViewer(group, SWT.BORDER | SWT.READ_ONLY);
		m_typeSelectionViewer.setContentProvider(new ArrayContentProvider());
		m_typeSelectionViewer.setLabelProvider(new SimpleLabelProvider());
		m_typeSelectionViewer.setSorter(new ViewerSorter());
		m_typeSelectionViewer.setInput(ETrendAdjustmentType.values());
		m_typeSelectionViewer.setSelection(new StructuredSelection(ETrendAdjustmentType.TIMESERIE_END));
		final Control control = m_typeSelectionViewer.getControl();
		control.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		m_typeSelectionViewer.getControl().setEnabled(true);

		new Label(group, SWT.NONE).setText("");
		m_copyButton = new Button(group, SWT.CHECK);
		m_copyButton.setText("Create a copy?");
		m_copyButton.setSelection(true);

	}

	public ETrendAdjustmentType getSelectedType() {
		StructuredSelection selection = (StructuredSelection) m_typeSelectionViewer.getSelection();
		Object object = selection.getFirstElement();
		if (object instanceof ETrendAdjustmentType) {
			return (ETrendAdjustmentType) object;
		}
		return null;
	}

	public boolean makeACopy() {
		return m_copyButton.getSelection();
	}

}