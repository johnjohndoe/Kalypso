package org.kalypso.statistics.tools;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.statistics.plugin.ImagesProvider;
import org.kalypso.statistics.types.EStatisticsImage;

public class FormToolkitHelper extends FormToolkit {

	public FormToolkitHelper(final Display display) {
		super(display);
	}

	public Composite createGroupWrappedComposite(final Composite parent, final String title, final int numColumns, final int marginWidth,
			final int marginHeight, final int horizontalSpacing, final int verticalSpacing, final boolean grabExcessHorizontalSpace,
			final boolean grabExcessVerticalSpace) {
		return createGroupWrappedComposite(parent, title, numColumns, marginWidth, marginHeight, horizontalSpacing, verticalSpacing, grabExcessHorizontalSpace,
				grabExcessVerticalSpace, 1, 1);
	}

	public Composite createGroupWrappedComposite(final Composite parent, final String title, final int numColumns, final int marginWidth,
			final int marginHeight, final int horizontalSpacing, final int verticalSpacing, final boolean grabExcessHorizontalSpace,
			final boolean grabExcessVerticalSpace, final int horizontalSpan, final int verticalSpan) {
		final Group group = new Group(parent, SWT.NONE);
		group.setBackground(parent.getBackground());
		group.setText(title);
		final GridLayout groupLayout = new GridLayout(1, false);
		groupLayout.marginWidth = 0;
		groupLayout.marginHeight = 0;
		groupLayout.horizontalSpacing = 0;
		groupLayout.verticalSpacing = 0;
		group.setLayout(groupLayout);
		final GridData layoutData = new GridData(grabExcessHorizontalSpace ? SWT.FILL : SWT.BEGINNING, grabExcessVerticalSpace ? SWT.FILL : SWT.DEFAULT,
				grabExcessHorizontalSpace, grabExcessVerticalSpace);
		layoutData.horizontalSpan = horizontalSpan;
		layoutData.verticalSpan = verticalSpan;
		group.setLayoutData(layoutData);
		final Composite composite = createComposite(group, SWT.NONE, numColumns, marginWidth, marginHeight, horizontalSpacing, verticalSpacing,
				grabExcessHorizontalSpace, grabExcessVerticalSpace);
		return composite;
	}

	public Composite createComposite(final Composite parent, final int style, final int numColumns, final int marginWidth, final int marginHeight,
			final int horizontalSpacing, final int verticalSpacing, final boolean grabExcessHorizontalSpace, final boolean grabExcessVerticalSpace) {
		return createComposite(parent, style, numColumns, marginWidth, marginHeight, horizontalSpacing, verticalSpacing, grabExcessHorizontalSpace,
				grabExcessVerticalSpace, 1, 1);
	}

	public Composite createComposite(final Composite parent, final int style, final int numColumns, final int marginWidth, final int marginHeight,
			final int horizontalSpacing, final int verticalSpacing, final boolean grabExcessHorizontalSpace, final boolean grabExcessVerticalSpace,
			final int horizontalSpan, final int verticalSpan) {
		final Composite composite = createComposite(parent, style);
		final GridLayout layout = new GridLayout(numColumns, false);
		layout.marginWidth = marginWidth;
		layout.marginHeight = marginHeight;
		layout.horizontalSpacing = horizontalSpacing;
		layout.verticalSpacing = verticalSpacing;
		composite.setLayout(layout);
		final GridData layoutData = new GridData(grabExcessHorizontalSpace ? SWT.FILL : SWT.BEGINNING, grabExcessVerticalSpace ? SWT.FILL : SWT.DEFAULT,
				grabExcessHorizontalSpace, grabExcessVerticalSpace);
		layoutData.horizontalSpan = horizontalSpan;
		layoutData.verticalSpan = verticalSpan;
		composite.setLayoutData(layoutData);
		return composite;
	}

	public Composite createComponentHolderComposite(final Composite parent, final int numColumns) {
		return createComponentHolderComposite(parent, SWT.NONE, numColumns, false, 1);
	}

	public Composite createComponentHolderComposite(final Composite parent, final int numColumns, final int horizontalSpan) {
		return createComponentHolderComposite(parent, SWT.NONE, numColumns, false, horizontalSpan);
	}

	public Composite createComponentHolderComposite(final Composite parent, final int style, final int numColumns, final boolean grabExcessVerticalSpace,
			final int horizontalSpan) {
		final Composite composite = createComposite(parent, style);
		final GridLayout layout = new GridLayout(numColumns, true);
		layout.marginWidth = 0;
		layout.marginHeight = 0;
		layout.horizontalSpacing = 0;
		layout.verticalSpacing = 0;
		composite.setLayout(layout);
		final GridData layoutData = new GridData(SWT.FILL, grabExcessVerticalSpace ? SWT.FILL : SWT.DEFAULT, true, grabExcessVerticalSpace);
		layoutData.horizontalSpan = horizontalSpan;
		composite.setLayoutData(layoutData);
		return composite;
	}

	public ToolBar createToolBar(final Composite parent) {
		return createToolBar(parent, true);
	}

	public ToolBar createToolBar(final Composite parent, final boolean grabExcessHorizontalSpace) {
		final ToolBar toolBar = new ToolBar(parent, SWT.FLAT);
		toolBar.setLayoutData(new GridData(grabExcessHorizontalSpace ? SWT.FILL : SWT.DEFAULT, SWT.TOP, grabExcessHorizontalSpace, false));
		return toolBar;
	}

	public ToolItem createToolItem(final ToolBar parent, final String label, final String tooltip, final EStatisticsImage image) {
		final ToolItem item = new ToolItem(parent, SWT.PUSH);
		if (label != null)
			item.setText(label);
		if (tooltip != null)
			item.setToolTipText(tooltip);
		if (image != null) {
			item.setImage(ImagesProvider.getImage(image));
			// image.dispose(); - kad se aktivira, sve slike su iste :)
		}
		return item;
	}

	public ToolItem createToolbarSeparator(final ToolBar parent) {
		return new ToolItem(parent, SWT.SEPARATOR);
	}

	public TabFolder createTabFolder(final Composite parent) {
		final TabFolder tabFolder = new TabFolder(parent, SWT.NONE);
		tabFolder.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		tabFolder.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				e.doit = false;
				tabFolder.traverse(SWT.TRAVERSE_TAB_NEXT);
			}

			@Override
			public void widgetDefaultSelected(final SelectionEvent e) {
			}
		});
		return tabFolder;
	}

	public TabItem createTabItem(final TabFolder parent, final String label) {
		final TabItem tabItem = new TabItem(parent, SWT.NONE);
		tabItem.setText(label);
		return tabItem;
	}

	@Override
	public Label createLabel(final Composite parent, final String text) {
		return createLabel(parent, text, false, 0);
	}

	public Label createLabel(final Composite parent, final int style, final String text, final boolean positionOnTop, final int topIndent) {
		final Label label = createLabel(parent, text, style);
		final GridData layoutData = new GridData(SWT.BEGINNING, positionOnTop ? SWT.TOP : SWT.CENTER, false, false);
		if (positionOnTop)
			layoutData.verticalIndent = topIndent;
		label.setLayoutData(layoutData);
		label.setBackground(parent.getBackground());
		return label;
	}

	public Label createLabel(final Composite parent, final String text, final boolean positionOnTop, final int topIndent) {
		return createLabel(parent, SWT.NONE, text, positionOnTop, topIndent);
	}

	@Override
	public Button createButton(final Composite parent, final String text, final int style) {
		final Button button = super.createButton(parent, text, style);
		button.setBackground(parent.getBackground());
		return button;
	}

	public Button createRadioButton(final Composite parent, final String text, final boolean grabExcessHorizontalSpace, final boolean setSelected) {
		final Button button = createButton(parent, text, SWT.RADIO);
		if (setSelected)
			button.setSelection(true);
		button.setLayoutData(new GridData(grabExcessHorizontalSpace ? SWT.FILL : SWT.DEFAULT, SWT.CENTER, grabExcessHorizontalSpace, false));
		button.setBackground(parent.getBackground());
		return button;
	}

	public Text createText(final Composite parent, final boolean grabExcessHorizontalSpace) {
		return createText(parent, SWT.BORDER, grabExcessHorizontalSpace);
	}

	public Text createText(final Composite parent, final int widthPart) {
		return createText(parent, SWT.BORDER, widthPart);
	}

	public Text createText(final Composite parent, final int style, final boolean grabExcessHorizontalSpace) {
		final Text text = createText(parent, "", style);
		text.setLayoutData(new GridData(grabExcessHorizontalSpace ? SWT.FILL : SWT.DEFAULT, SWT.CENTER, grabExcessHorizontalSpace, false));
		return text;
	}

	public Text createText(final Composite parent, final int style, final int widthPart) {
		if (widthPart < 2)
			return createText(parent, SWT.BORDER, true);
		final Composite composite = createComponentHolderComposite(parent, widthPart);
		final Text text = createText(composite, style, true);
		for (int i = 1; i < widthPart; i++)
			createLabel(composite, "");
		return text;
	}

	public Text createTextArea(final Composite parent, final boolean grabExcessHorizontalSpace, final int heightHint, final int textLimit,
			final boolean addTabTraversalBehavior) {
		final Text text = createText(parent, "", SWT.BORDER | SWT.MULTI | SWT.WRAP | SWT.V_SCROLL);
		final GridData layout = new GridData(grabExcessHorizontalSpace ? SWT.FILL : SWT.BEGINNING, SWT.TOP, grabExcessHorizontalSpace, false);
		layout.heightHint = heightHint;
		text.setLayoutData(layout);
		if (textLimit > 0)
			text.setTextLimit(textLimit);
		return text;
	}

	public Combo createCombo(final Composite parent, final boolean readOnly, final boolean grabExcessHorizontalSpace) {
		final int style = readOnly ? SWT.FLAT | SWT.BORDER | SWT.READ_ONLY : SWT.FLAT | SWT.BORDER;
		final Combo combo = new Combo(parent, style);
		combo.setLayoutData(new GridData(grabExcessHorizontalSpace ? SWT.FILL : SWT.DEFAULT, SWT.CENTER, grabExcessHorizontalSpace, false));
		return combo;
	}

	public Combo createCombo(final Composite parent, final boolean readOnly, final int widthPart) {
		if (widthPart < 2)
			return createCombo(parent, readOnly, true);
		final Composite composite = createComponentHolderComposite(parent, widthPart);
		final Combo combo = createCombo(composite, readOnly, true);
		for (int i = 1; i < widthPart; i++)
			createLabel(composite, "");
		return combo;
	}

	public Spinner createSpinner(final Composite parent, final boolean grabExcessHorizontalSpace, final int minValue, final int maxValue,
			final int defaultValue, final int incrementValue, final int pageIncrementValue, final int numberOfDecimalDigits) {
		final Spinner spinner = new Spinner(parent, SWT.BORDER);
		spinner.setValues(defaultValue, minValue, maxValue, numberOfDecimalDigits, incrementValue, pageIncrementValue);
		final GridData layout = new GridData(grabExcessHorizontalSpace ? SWT.FILL : SWT.BEGINNING, SWT.TOP, grabExcessHorizontalSpace, false);
		spinner.setLayoutData(layout);
		return spinner;
	}

	public GridData createGridData(final int horizontalSpan, final int verticalSpan, final boolean grabExcessHorizontalSpace,
			final boolean grabExcessVerticalSpace) {
		final GridData gridData = new GridData();
		gridData.horizontalSpan = horizontalSpan;
		gridData.verticalSpan = verticalSpan;
		gridData.horizontalAlignment = grabExcessHorizontalSpace ? SWT.FILL : SWT.BEGINNING;
		gridData.grabExcessHorizontalSpace = grabExcessHorizontalSpace;
		gridData.verticalAlignment = grabExcessVerticalSpace ? SWT.FILL : SWT.TOP;
		gridData.grabExcessVerticalSpace = grabExcessVerticalSpace;
		return gridData;
	}

	public Label createHorizontalSeparator(final Composite parent, final boolean grabExcessHorizontalSpace) {
		final Label separator = new Label(parent, SWT.HORIZONTAL | SWT.SEPARATOR);
		separator.setLayoutData(new GridData(grabExcessHorizontalSpace ? GridData.FILL_HORIZONTAL : GridData.BEGINNING));
		return separator;
	}

}
