package org.kalypso.dss.planerclient.browser;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IPerspectiveRegistry;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.WorkbenchException;
import org.eclipse.ui.XMLMemento;
import org.eclipse.ui.internal.IWorkbenchConstants;
import org.eclipse.ui.internal.WorkbenchPage;
import org.eclipse.ui.internal.WorkbenchWindow;
import org.eclipse.ui.internal.misc.StatusUtil;

public class KalypsoPerspectiveHelper {

	/**
	 * Diese Methode wechselt zur nächsten Perspective
	 */
	public static boolean nextPerspective(String urlWithPageState,
			String perspectiveID, IWorkbenchPage activePage) {

		final IWorkbench workbench = PlatformUI.getWorkbench();

		final IPerspectiveRegistry registry = workbench
				.getPerspectiveRegistry();

		WorkbenchPage page = null;

		try {

			XMLMemento memento = XMLMemento.createReadRoot(new FileReader(
					urlWithPageState));
			IMemento windowMemento = memento
					.getChild(IWorkbenchConstants.TAG_WINDOW);
			IMemento pageMemento = windowMemento
					.getChild(IWorkbenchConstants.TAG_PAGE);

			// TODO this might be usefull if the implemenation changes
			// IMemento perspspectiveMemento = pageMemento
			// .getChild(IWorkbenchConstants.TAG_PERSPECTIVES);
			// IMemento singlePerspective = perspspectiveMemento
			// .getChild(IWorkbenchConstants.TAG_PERSPECTIVE);
			// IMemento descMemento = singlePerspective
			// .getChild(IWorkbenchConstants.TAG_DESCRIPTOR);
			// String perspectiveID = descMemento
			// .getString(IWorkbenchConstants.TAG_ID);

			IPerspectiveDescriptor realDesc = registry
					.findPerspectiveWithId(perspectiveID);
			IPerspectiveDescriptor oldPerspectiveDesc = activePage
					.getPerspective();
			activePage.closePerspective(oldPerspectiveDesc, true, false);
			// IWorkbenchWindow activWorkbenchWindow = workbench
			// .getActiveWorkbenchWindow();
			// page = (WorkbenchPage) activWorkbenchWindow.openPage(realDesc
			// .getId(), null);

			// Wieso diese Reihenfolge?? aber funktioniert
			// page.restoreState(pageMemento, realDesc);
			// page.setPerspective(realDesc);
			// page.resetPerspective();
			((WorkbenchPage) activePage).restoreState(pageMemento, null);
			activePage.setPerspective(realDesc);
			activePage.resetPerspective();

		} catch (IOException e) {
			e.printStackTrace();
			handleException(e, page);
			return false;
		} catch (WorkbenchException e) {
			e.printStackTrace();
			handleException(e, page);
			return false;
		}

		return true;
	}

	/**
	 * Handles workbench exception
	 */
	private static void handleException(Exception e, WorkbenchPage page) {
		ErrorDialog.openError(page.getActivePart().getSite().getShell(),
				"Link Error", e.getMessage(), StatusUtil.newStatus(
						IStatus.WARNING, e.getMessage(), e));
	}

	/**
	 * Diese Methode wechselt die Perspective.
	 * 
	 * Problem: bevor die nächste Perspective eingeblendet wird, verkleinert
	 * sich das Fenster um dann gleich wieder gross zu werden(unruhig für den
	 * Benutzer)
	 */
	public static void nextPerspective(String url, String perspectiveID)
			throws WorkbenchException, FileNotFoundException {

		// read Memento
		XMLMemento memento = XMLMemento.createReadRoot(new FileReader(url));
		IMemento windowMemento = memento
				.getChild(IWorkbenchConstants.TAG_WINDOW);

		IWorkbench workbench = PlatformUI.getWorkbench();
		IPerspectiveRegistry reg = workbench.getPerspectiveRegistry();
		IPerspectiveDescriptor persDesc = reg
				.findPerspectiveWithId(perspectiveID);
		if (persDesc == null)
			persDesc = reg.findPerspectiveWithId(reg.getDefaultPerspective());
		WorkbenchWindow window = (WorkbenchWindow) workbench
				.getActiveWorkbenchWindow();
		IWorkbenchPage activePage = window.getActivePage();
		IPerspectiveDescriptor oldDesc = activePage.getPerspective();
		activePage.closePerspective(oldDesc, true, false);
		window.restoreState(windowMemento, persDesc);
	}
}
