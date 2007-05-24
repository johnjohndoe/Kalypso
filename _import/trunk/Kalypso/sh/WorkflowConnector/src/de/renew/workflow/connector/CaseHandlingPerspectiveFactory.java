package de.renew.workflow.connector;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

public class CaseHandlingPerspectiveFactory implements IPerspectiveFactory
{
  public static final String SCENARIO_VIEW_ID = "de.renew.workflow.connector.CaseView";

  public void createInitialLayout( final IPageLayout layout )
  {
    // Get the editor area.
    final String editorArea = layout.getEditorArea();

    final IFolderLayout leftTop = layout.createFolder( "leftTop", IPageLayout.LEFT, 0.3f, editorArea );
    leftTop.addView( WorklistView.ID );
    layout.getViewLayout( WorklistView.ID ).setCloseable( false );
    layout.getViewLayout( WorklistView.ID ).setMoveable( false );

    final IFolderLayout leftBottom = layout.createFolder( "leftBottom", IPageLayout.BOTTOM, 0.7f, "leftTop" );
    leftBottom.addView( SCENARIO_VIEW_ID );
    layout.getViewLayout( SCENARIO_VIEW_ID ).setCloseable( false );
    layout.getViewLayout( SCENARIO_VIEW_ID ).setMoveable( false );
  }

}
