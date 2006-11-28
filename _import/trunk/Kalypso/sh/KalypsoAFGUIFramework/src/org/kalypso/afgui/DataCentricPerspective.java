/**
 * 
 */
package org.kalypso.afgui;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.eclipse.ui.internal.Perspective;
import org.kalypso.afgui.views.ModelDataView;
import org.kalypso.afgui.views.WorkflowHelpView;
import org.kalypso.afgui.views.WorkflowView;

/**
 * @author congo
 *
 */
public class DataCentricPerspective implements IPerspectiveFactory {

	public void createInitialLayout(IPageLayout layout) {
		
//		 // Get the editor area.
//		 String editorArea = layout.getEditorArea();
//
//		 // Top left: Resource Navigator view and Bookmarks view placeholder
//		 IFolderLayout topLeft = 
//			 	layout.createFolder("topLeft", IPageLayout.LEFT, 0.45f,
//		    editorArea);
//		 topLeft.addView(ModelDataView.ID);//IPageLayout.ID_BOOKMARKS);
//		 //topLeft.addView();//IPageLayout.ID_RES_NAV);
//		 //topLeft.addView(IPageLayout.ID_RES_NAV);
//		 //topLeft.addPlaceholder(IPageLayout.ID_BOOKMARKS);
//
//		 // Bottom left: Outline view and Property Sheet view
//		 IFolderLayout bottomLeft = 
//			 layout.createFolder(
//					 	"bottomLeft", 
//					 	IPageLayout.BOTTOM, 0.50f,
//		 	   			"topLeft");
////		 bottomLeft.addView(
////				 		IPageLayout.ID_OUTLINE);
//		 bottomLeft.addView(
//				 		WorkflowView.ID);//IPageLayout.ID_PROP_SHEET);
//		 //bottomLeft.addView(WorkflowView.ID);
//		 // Bottom right: Task List view
//		 layout.addView(
//				 	IPageLayout.ID_TASK_LIST, 
//				 	IPageLayout.BOTTOM, 
//				 	0.66f, 
//				 	editorArea);
	}
}
