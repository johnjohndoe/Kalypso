package org.kalypso.portal.perspective;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

public class KalypsoPlanerPortalPerspectiveFactory implements
		IPerspectiveFactory {

	public KalypsoPlanerPortalPerspectiveFactory() {
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

		/**
		 * <em>                
		 *  ====================================
		 *  #      editor            |navigator# 
		 *  #                        |         #
		 *  #                        |         #
		 *  #                        |         #
		 *  #                        |-------- #
		 *  #                        |gazetter #
		 *  #                        |         #
		 *  #------------------------|         #
		 *  # control  | outline     |         #
		 *  # (browser)|             |         #
		 *  #          |             |         #
		 *  ====================================
		 *  </em>
		 */
		// Top Right.
		String idNavigator = "navigator";
		String idGazetteer = "gazetteer";
		String idOutline = "outline";
		String idControl = "control";
		IFolderLayout rightSide = layout.createFolder(idNavigator,
				IPageLayout.RIGHT, (float) 0.75, editorArea);//$NON-NLS-1$
		rightSide.addView(IPageLayout.ID_RES_NAV);

		// Bottom Right.
		IFolderLayout bottomRight = layout.createFolder(idGazetteer,
				IPageLayout.BOTTOM, (float) 0.50,//$NON-NLS-1$
				idNavigator);//$NON-NLS-1$
		bottomRight.addView("KalypsoPortal.view.gazatteer");

		IFolderLayout bottomCenter = layout.createFolder(idOutline,
				IPageLayout.BOTTOM, (float) 0.70, editorArea);
		bottomCenter.addView(IPageLayout.ID_OUTLINE);

		// Bottom right.
		IFolderLayout bottomLeft = layout.createFolder(idControl,
				IPageLayout.LEFT, (float) 0.66,//$NON-NLS-1$
				idOutline);

		// bottomRight.addPlaceholder(CommandURLBrowserView.WEB_BROWSER_VIEW_ID);
//		bottomLeft.addView("Kalypso.CommandURLBrowserView");
		bottomLeft.addView("org.kalypso.workflow.ui.WorkflowBrowserView");

		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.folder");//$NON-NLS-1$
		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.file");//$NON-NLS-1$
	}

}
