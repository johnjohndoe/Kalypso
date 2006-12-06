package org.kalypso.kalypso1d2d.pjt.perspective;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DNewProjectWizard;
import org.kalypso.kalypso1d2d.pjt.views.ActivitiesView;
import org.kalypso.kalypso1d2d.pjt.views.SimulationModelDBView;
import org.kalypso.kalypso1d2d.pjt.views.TasksView;
import org.kalypso.kalypso1d2d.pjt.views.WorkflowView;



public class Perspective implements IPerspectiveFactory {
	
	final static public String ID="org.kalypso.kalypso1d2d.pjt.perspective.Perspective";
	
	public void createInitialLayout(IPageLayout layout) {
		
		 // Get the editor area.
		 String editorArea = layout.getEditorArea();

		 // Top left: Resource Navigator view and Bookmarks view placeholder
		 IFolderLayout leftTop = 
			 	layout.createFolder(
			 				"leftTop", 
			 				IPageLayout.LEFT, 
			 				0.2f,
			 				editorArea);
		//with left middle and bottom 
//		 IFolderLayout leftMiddle = 
//			 	layout.createFolder(
//			 				"leftMiddle", 
//			 				IPageLayout.BOTTOM, 
//			 				0.4f,
//			 				"leftTop");
//		 
//		 IFolderLayout leftBottom = 
//			 	layout.createFolder(
//			 				"leftBottom", 
//			 				IPageLayout.BOTTOM, 
//			 				0.5f,
//			 				"leftMiddle");
		 
		 //only leftbottom
		 IFolderLayout leftBottom = 
			 	layout.createFolder(
			 				"leftBottom", 
			 				IPageLayout.BOTTOM, 
			 				0.3f,
			 				"leftTop");
		 
		 IFolderLayout bottom = 
			 	layout.createFolder(
			 				"bottom", 
			 				IPageLayout.BOTTOM, 
			 				0.80f,
			 				editorArea);
		 IFolderLayout rightTop = 
			 	layout.createFolder(
			 				"rightTop", 
			 				IPageLayout.RIGHT, 
			 				0.7f,
			 				editorArea);
		 IFolderLayout rightMiddle = 
			 	layout.createFolder(
			 				"rightMiddle", 
			 				IPageLayout.BOTTOM, 
			 				0.5f,
			 				"rightTop");
		 
		 leftTop.addView(WorkflowView.ID);//IPageLayout.ID_RES_NAV);
		 //leftMiddle.addView(TasksView.ID);
		 leftBottom.addView(IPageLayout.ID_OUTLINE);
		 bottom.addView(IPageLayout.ID_PROBLEM_VIEW);
		 //bottom.addView(IPageLayout.ID_)
		 rightTop.addView(SimulationModelDBView.ID);
		 rightTop.addView(IPageLayout.ID_RES_NAV);
		 rightMiddle.addView(ActivitiesView.ID);
		 layout.addNewWizardShortcut(Kalypso1D2DNewProjectWizard.ID);
	}
}
