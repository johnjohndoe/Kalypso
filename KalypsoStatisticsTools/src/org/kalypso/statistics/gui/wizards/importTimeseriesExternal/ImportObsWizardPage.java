package org.kalypso.statistics.gui.wizards.importTimeseriesExternal;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.TimeZone;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.ui.forms.MessageProvider;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.sensor.adapter.INativeObservationAdapter;
import org.kalypso.statistics.gui.NodeLabelProvider;
import org.kalypso.statistics.gui.views.nodes.NodesContentProvider;
import org.kalypso.statistics.project.SessionDataProvider;
import org.kalypso.statistics.types.data.NodeProfile;
import org.kalypso.zml.ui.imports.ObservationImportSelection;

public class ImportObsWizardPage extends WizardPage implements /*
																 * FocusListener,
																 */ISelectionProvider, ISelectionChangedListener {
	private final List<ISelectionChangedListener> m_selectionListener = new ArrayList<ISelectionChangedListener>();

	private final List<INativeObservationAdapter> m_adapter;

	private ComboViewer m_formatCombo;

	private boolean m_controlFinished = false;

	private TimeZone m_timezone;

	private String m_sourcePath;

	private ComboViewer m_parentNodeSelectionViewer;

	public ImportObsWizardPage(final String pageName) {
		this(pageName, null, null);
		m_timezone = KalypsoCorePlugin.getDefault().getTimeZone();
	}

	public ImportObsWizardPage(final String pageName, final String title, final ImageDescriptor titleImage) {
		super(pageName, title, titleImage);
		setTitle("Timeseries import");
		setDescription("Import timeseries data from different formats");
		setPageComplete(false);

		m_adapter = createNativeAdapters();
	}

	// FIXME: move into spearate extension class
	private List<INativeObservationAdapter> createNativeAdapters() {
		final List<INativeObservationAdapter> adapters = new ArrayList<INativeObservationAdapter>();

		final IExtensionRegistry registry = Platform.getExtensionRegistry();

		final IExtensionPoint extensionPoint = registry.getExtensionPoint("org.kalypso.core.nativeObsAdapter"); //$NON-NLS-1$

		if (extensionPoint == null)
			return adapters;

		final IExtension[] extensions = extensionPoint.getExtensions();
		for (final IExtension extension : extensions) {
			final IConfigurationElement[] elements = extension.getConfigurationElements();

			for (final IConfigurationElement element : elements) {
				try {
					final INativeObservationAdapter adapter = (INativeObservationAdapter) element.createExecutableExtension("class"); //$NON-NLS-1$
					adapters.add(adapter);
				} catch (final CoreException e) {
					e.printStackTrace();
				}
			}
		}

		return adapters;
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
		m_controlFinished = true;
	}

	public void createControlSource(final Composite parent) {
		final Group group = new Group(parent, SWT.NONE);
		group.setLayout(new GridLayout(3, false));
		group.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
		group.setText("Source");

		// line 1
		final Label label = new Label(group, SWT.NONE);
		label.setText("File");

		final Text textFileSource = new Text(group, SWT.BORDER);
		textFileSource.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		textFileSource.setText(StringUtils.EMPTY);
		textFileSource.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(final ModifyEvent e) {
				handleSourcePathModified(textFileSource.getText());
			}
		});

		final Button button = new Button(group, SWT.PUSH);
		button.setText("..."); //$NON-NLS-1$
		button.setLayoutData(new GridData(SWT.END, SWT.CENTER, false, false));

		button.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				chooseSourceFile(textFileSource);
			}
		});

		// line 2
		final Label formatLabel = new Label(group, SWT.NONE);
		formatLabel.setText("Format");

		m_formatCombo = new ComboViewer(group, SWT.DROP_DOWN | SWT.READ_ONLY);
		m_formatCombo.getControl().setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		m_formatCombo.setContentProvider(new ArrayContentProvider());
		m_formatCombo.setLabelProvider(new LabelProvider());
		m_formatCombo.setInput(m_adapter);

		m_formatCombo.addSelectionChangedListener(this);

		if (m_adapter.size() > 0)
			m_formatCombo.setSelection(new StructuredSelection(m_adapter.get(0)));

		new Label(group, SWT.NONE);

		// TimeZone
		/* time zone selection */
		final Label timezoneLabel = new Label(group, SWT.NONE);
		timezoneLabel.setText("Time Zone");

		final String[] tz = TimeZone.getAvailableIDs();
		Arrays.sort(tz);

		final ComboViewer comboTimeZones = new ComboViewer(group, SWT.BORDER | SWT.SINGLE);
		comboTimeZones.getControl().setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false));

		comboTimeZones.setContentProvider(new ArrayContentProvider());
		comboTimeZones.setLabelProvider(new LabelProvider());
		comboTimeZones.setInput(tz);

		comboTimeZones.addFilter(new ViewerFilter() {
			@Override
			public boolean select(final Viewer viewer, final Object parentElement, final Object element) {
				if (element instanceof String) {
					final String name = (String) element;
					return !name.toLowerCase().startsWith("etc/"); //$NON-NLS-1$
				}

				return true;
			}
		});

		comboTimeZones.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(final SelectionChangedEvent event) {
				final IStructuredSelection selection = (IStructuredSelection) comboTimeZones.getSelection();
				updateTimeZone((String) selection.getFirstElement());
			}
		});

		comboTimeZones.getCombo().addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(final ModifyEvent e) {
				updateTimeZone(comboTimeZones.getCombo().getText());
			}
		});

		if (m_timezone != null) {
			final String id = m_timezone.getID();
			if (ArrayUtils.contains(tz, id))
				comboTimeZones.setSelection(new StructuredSelection(id));
			else
				comboTimeZones.getCombo().setText(id);
		}
		new Label(group, SWT.NONE);

		// node selection
		new Label(group, SWT.NONE).setText("Node / Station");
		m_parentNodeSelectionViewer = new ComboViewer(group, SWT.BORDER | SWT.READ_ONLY);
		m_parentNodeSelectionViewer.setContentProvider(new NodesContentProvider());
		m_parentNodeSelectionViewer.setLabelProvider(new NodeLabelProvider());
		m_parentNodeSelectionViewer.setSorter(new ViewerSorter());
		m_parentNodeSelectionViewer.setInput(SessionDataProvider.getInstance().getDataProvider().getNodes());
		m_parentNodeSelectionViewer.setSelection(new StructuredSelection(SessionDataProvider.getInstance().getDataProvider().getNodes().get(0)));
		final Control control = m_parentNodeSelectionViewer.getControl();
		control.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));
		m_parentNodeSelectionViewer.getControl().setEnabled(true);

	}

	public NodeProfile getParentNode() {
		StructuredSelection selection = (StructuredSelection) m_parentNodeSelectionViewer.getSelection();
		Object object = selection.getFirstElement();
		if (object instanceof NodeProfile) {
			return (NodeProfile) object;
		}
		return null;
	}

	protected void handleSourcePathModified(final String sourcePath) {
		m_sourcePath = sourcePath;

		validate();
	}

	protected void chooseSourceFile(final Text textFileSource) {
		final FileDialog dialog = new FileDialog(getShell(), SWT.SINGLE);

		final File sourceFile = getSourceFile();
		if (sourceFile != null) {
			dialog.setFileName(sourceFile.getName());
			dialog.setFilterPath(sourceFile.getParent());
		}

		if (dialog.open() == null)
			return;

		final String fileName = dialog.getFileName();
		final String filterPath = dialog.getFilterPath();
		final File newSourceFile = new File(filterPath, fileName);
		textFileSource.setText(newSourceFile.getAbsolutePath());
	}

	private File getSourceFile() {
		if (StringUtils.isBlank(m_sourcePath))
			return null;

		return new File(m_sourcePath);
	}

	/**
	 * validates the page
	 */
	void validate() {
		// Do not validate until page was created
		if (!m_controlFinished)
			return;

		final IMessageProvider message = doValidate();
		if (message == null)
			setMessage(null);
		else
			setMessage(message.getMessage(), message.getMessageType());
		setPageComplete(message == null);
		fireSelectionChanged();
	}

	private IMessageProvider doValidate() {
		final File sourceFile = getSourceFile();
		if (sourceFile == null)
			return new MessageProvider("Source not selected", ERROR);
		if (!sourceFile.isFile())
			return new MessageProvider("Source is not a valid file", ERROR); //$NON-NLS-1$
		if (m_timezone == null)
			return new MessageProvider("Please select a valid time zone", ERROR); //$NON-NLS-1$
		return null;
	}

	private void fireSelectionChanged() {
		for (final Object element : m_selectionListener)
			((ISelectionChangedListener) element).selectionChanged(new SelectionChangedEvent(this, getSelection()));
	}

	/**
	 * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
	 */
	@Override
	public void addSelectionChangedListener(final ISelectionChangedListener listener) {
		m_selectionListener.add(listener);
	}

	/**
	 * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
	 */
	@Override
	public ISelection getSelection() {
		final IStructuredSelection formatSelection = (IStructuredSelection) m_formatCombo.getSelection();
		if (!m_controlFinished)
			return StructuredSelection.EMPTY;

		final File sourceFile = getSourceFile();
		return new ObservationImportSelection(sourceFile, null, (INativeObservationAdapter) formatSelection.getFirstElement(), false, false, m_timezone);
	}

	/**
	 * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
	 */
	@Override
	public void removeSelectionChangedListener(final ISelectionChangedListener listener) {
		m_selectionListener.remove(listener);
	}

	/**
	 * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
	 */
	@Override
	public void setSelection(final ISelection selection) {
		if (selection instanceof ObservationImportSelection) {
			final ObservationImportSelection s = (ObservationImportSelection) selection;
			if (m_formatCombo != null)
				m_formatCombo.setSelection(new StructuredSelection(s.getNativeAdapter()));

			final File sourceFile = s.getFileSource();
			if (sourceFile == null)
				m_sourcePath = null;
			else
				m_sourcePath = sourceFile.getAbsolutePath();
		}
	}

	/**
	 * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
	 */
	@Override
	public void selectionChanged(final SelectionChangedEvent event) {
		fireSelectionChanged();
	}

	protected void updateTimeZone(final String timeZoneID) {
		m_timezone = null;

		if (timeZoneID != null) {
			final TimeZone timeZone = TimeZone.getTimeZone(timeZoneID.toUpperCase());
			// Only set, if timezone could be parsed
			if (!timeZone.getID().equals("GMT") || timeZoneID.toUpperCase().equals("GMT")) //$NON-NLS-1$ //$NON-NLS-2$
				m_timezone = timeZone;
		}

		validate();
	}

}