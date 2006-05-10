package org.kalypso.portal.dialog;

import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.target.internal.ui.SiteExplorerView;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.PartInitException;

public class SiteDialog extends TitleAreaDialog {

	private IWorkbenchPartSite m_site;

	private final SiteExplorerView m_view;

	public SiteDialog(Shell parentShell, IWorkbenchPartSite site) {
		super(parentShell);
		m_site = site;
		m_view = new SiteExplorerView();
		setTitle("Projekt Explorer");
		setShellStyle(getShellStyle() | SWT.MAX);
	}

	protected Control createDialogArea(Composite parent) {
		Composite main = (Composite) super.createDialogArea(parent);
		GridLayout layout = new GridLayout(1, true);
		GridData gd = new GridData();
		gd.heightHint = 300;
		gd.widthHint = 500;
		main.setLayout(layout);
		main.setLayoutData(gd);
		if (m_site instanceof IWorkbenchPartSite) {
			try {
				m_view.init((IViewSite) m_site);
			} catch (PartInitException e) {
				e.printStackTrace();
			}
		}
		m_view.createPartControl(main);

		return main;
	}
}
