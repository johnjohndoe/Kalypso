package org.kalypso.dss.planerclient.actions;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.XMLMemento;
import org.eclipse.ui.internal.IWorkbenchConstants;
import org.eclipse.ui.internal.WorkbenchPage;
import org.eclipse.ui.internal.registry.PerspectiveDescriptor;
import org.kalypso.dss.KalypsoDSSPlugin;

public class SavePrespectivePageAction implements
		IWorkbenchWindowActionDelegate {

	private static IWorkbenchWindow m_window;

	public static final String TAG_KALYPSO_PERSPECTIVE = "rootnode";

	public static final String[] FILTER_EXTENSION = new String[] { "*.xml" };

	public void dispose() {
		// TODO Auto-generated method stub

	}

	public void init(IWorkbenchWindow window) {
		m_window = window;
	}

	public void run(IAction action) {

		FileDialog dialog = new FileDialog(m_window.getShell(), SWT.SAVE);
		dialog.setFilterExtensions(FILTER_EXTENSION);
		dialog.setText("Save State to file..");
		dialog.setFilterPath(System.getProperty("TEMP"));
		String result = dialog.open();

		if (result == null)
			return;
		File targetFile = new File(result);

		IWorkbench workbench = PlatformUI.getWorkbench();

		WorkbenchPage activePage = (WorkbenchPage) workbench
				.getActiveWorkbenchWindow().getActivePage();
	
		PerspectiveDescriptor perspectiveDesc = (PerspectiveDescriptor) activePage
				.getPerspective();

		// create new Memento to store elements in
		XMLMemento root = XMLMemento.createWriteRoot(TAG_KALYPSO_PERSPECTIVE);
		final MultiStatus statusCollector = new MultiStatus(
				KalypsoDSSPlugin.PLUGIN_ID, IStatus.OK, "", null);
		// save stati
		statusCollector.merge(perspectiveDesc.saveState(root
				.createChild(IWorkbenchConstants.TAG_PERSPECTIVE)));
		statusCollector.merge(activePage.saveState(root));
		try {
			root.save(new FileWriter(targetFile));
		} catch (IOException e) {
			e.printStackTrace();
		}
		System.out.println(statusCollector.getMessage());

	}

	public void selectionChanged(IAction action, ISelection selection) {
		// TODO Auto-generated method stub

	}

}
