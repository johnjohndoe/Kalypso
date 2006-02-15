package org.kalypso.dss.planerclient.perspectives;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.kalypso.dss.planerclient.browser.KalypsoWebBrowserView;

public class KalypsoDssPlanerPhaseTwoClientPerspectiveFactory implements
		IPerspectiveFactory {

	public KalypsoDssPlanerPhaseTwoClientPerspectiveFactory() {
		super();
		// TODO Auto-generated constructor stub
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
		IFolderLayout topRight = layout.createFolder(
				"topRight", IPageLayout.RIGHT, (float) 0.85, editorArea);//$NON-NLS-1$
		topRight.addView(IPageLayout.ID_RES_NAV);

		// Bottom right.
		IFolderLayout bottomRight = layout.createFolder(
				"bottomRight", IPageLayout.BOTTOM, (float) 0.66,//$NON-NLS-1$
				editorArea);

		// bottomRight.addPlaceholder(KalypsoWebBrowserView.WEB_BROWSER_VIEW_ID);
		bottomRight.addView(KalypsoWebBrowserView.WEB_BROWSER_VIEW_ID);

		// setContentsOfShowViewMenu( layout );

		// layout.addActionSet( "org.kalypso.actionSet.model" );

		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.folder");//$NON-NLS-1$
		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.file");//$NON-NLS-1$

	}

	/**
	 * Sets the intial contents of the "Show View" menu
	 * 
	 * F*
	 * 
	 * @param layout
	 */
	protected void setContentsOfShowViewMenu(IPageLayout layout) {
		layout.addShowViewShortcut(IPageLayout.ID_RES_NAV);
		layout.addShowViewShortcut(IPageLayout.ID_OUTLINE);
	}
}
