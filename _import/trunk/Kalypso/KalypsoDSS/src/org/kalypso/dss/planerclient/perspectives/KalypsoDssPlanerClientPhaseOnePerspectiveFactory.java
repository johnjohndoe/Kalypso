package org.kalypso.dss.planerclient.perspectives;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.kalypso.dss.planerclient.browser.KalypsoWebBrowserView;

public class KalypsoDssPlanerClientPhaseOnePerspectiveFactory implements
		IPerspectiveFactory {

	public KalypsoDssPlanerClientPhaseOnePerspectiveFactory() {
	}

	public void createInitialLayout(IPageLayout layout) {
		defineLayout(layout);

	}

	/**
	 * Defines the initial layout for a page.
	 */
	protected void defineLayout(IPageLayout layout) {

		// Editors are placed for free.
		final String editorArea = layout.getEditorArea();

		// Top left.
		IFolderLayout topLeft = layout.createFolder(
				"topLeft", IPageLayout.LEFT, (float) 0.26, editorArea);//$NON-NLS-1$
		topLeft.addView(IPageLayout.ID_RES_NAV);
		topLeft.addPlaceholder(IPageLayout.ID_BOOKMARKS);

		// Bottom left.
		IFolderLayout bottomLeft = layout.createFolder(
				"bottomLeft", IPageLayout.BOTTOM, (float) 0.50,//$NON-NLS-1$
				"topLeft");//$NON-NLS-1$
		bottomLeft.addView(IPageLayout.ID_OUTLINE);

		// Bottom right.
		IFolderLayout bottomRight = layout.createFolder(
				"bottomRight", IPageLayout.BOTTOM, (float) 0.66,//$NON-NLS-1$
				editorArea);

//		bottomRight.addPlaceholder(KalypsoWebBrowserView.WEB_BROWSER_VIEW_ID);
		 bottomRight.addView(KalypsoWebBrowserView.WEB_BROWSER_VIEW_ID);

		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.folder");//$NON-NLS-1$
		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.file");//$NON-NLS-1$
	}

}
