package org.kalypso.statistics.gui.wizards.createExtremsTimeseries;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
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
import org.kalypso.statistics.types.EHydrologicalInterval;

public class CreateExtemsTimeseriesWizardPage extends WizardPage {

	private ComboViewer m_intervalTypeSelectionViewer;
	private Button m_coefficientButton;

	public CreateExtemsTimeseriesWizardPage(final String pageName) {
		this(pageName, null, null);
	}

	public CreateExtemsTimeseriesWizardPage(final String pageName, final String title, final ImageDescriptor titleImage) {
		super(pageName, title, titleImage);
		setTitle("Create Extems Timeseries");
		setDescription("Create extrems timeseries");
	}

	/**
	 * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
	 */
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
		group.setText("Select calculation period type");

		new Label(group, SWT.NONE).setText("Type");
		m_intervalTypeSelectionViewer = new ComboViewer(group, SWT.BORDER | SWT.READ_ONLY);
		m_intervalTypeSelectionViewer.setContentProvider(new ArrayContentProvider());
		m_intervalTypeSelectionViewer.setLabelProvider(new SimpleLabelProvider());
		m_intervalTypeSelectionViewer.setSorter(new ViewerSorter());
		m_intervalTypeSelectionViewer.setInput(EHydrologicalInterval.values());
		m_intervalTypeSelectionViewer.setSelection(new StructuredSelection(EHydrologicalInterval.CALENDAR_YEAR));
		final Control control = m_intervalTypeSelectionViewer.getControl();
		control.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		m_intervalTypeSelectionViewer.getControl().setEnabled(true);

		new Label(group, SWT.NONE).setText("");
		m_coefficientButton = new Button(group, SWT.CHECK);
		m_coefficientButton.setText("Coefficient 'e' over the whole period?");
		m_coefficientButton.setEnabled(false);

		m_intervalTypeSelectionViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				ISelection selection = event.getSelection();
				if (selection instanceof IStructuredSelection) {
					Object object = ((IStructuredSelection) selection).getFirstElement();
					if (object instanceof EHydrologicalInterval) {
						EHydrologicalInterval interval = (EHydrologicalInterval) object;
						switch (interval) {
						case CALENDAR_YEAR:
						case HYDROLOGIC_YEAR_NH:
							m_coefficientButton.setEnabled(false);
							break;
						case HYDROLOGIC_SUMMER_NH:
						case HYDROLOGIC_WINTER_NH:
							m_coefficientButton.setEnabled(true);
							break;
						}
					}
				}
			}
		});
	}

	public EHydrologicalInterval getSelectedIntervalType() {
		StructuredSelection selection = (StructuredSelection) m_intervalTypeSelectionViewer.getSelection();
		Object object = selection.getFirstElement();
		if (object instanceof EHydrologicalInterval) {
			return (EHydrologicalInterval) object;
		}
		return null;
	}

	public boolean coefficientOverTheWholePeriod() {
		return m_coefficientButton.getSelection();
	}

}