package org.kalypso.kalypso1d2d.pjt.perspective;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.eclipse.ui.IPlaceholderFolderLayout;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DNewProjectWizard;
import org.kalypso.kalypso1d2d.pjt.views.SimulationModelDBView;
import org.kalypso.kalypso1d2d.pjt.views.WorkflowView;
import org.kalypso.ogc.gml.outline.GisMapOutlineView;
import org.kalypso.ui.editor.featureeditor.FeatureTemplateView;
import org.kalypso.ui.editor.mapeditor.views.ActionOptionsView;
import org.kalypso.ui.views.map.MapView;

public class Perspective implements IPerspectiveFactory
{

  final static public String ID = "org.kalypso.kalypso1d2d.pjt.perspective.Perspective";

  public void createInitialLayout( final IPageLayout layout )
  {

    // Get the editor area.
    final String editorArea = layout.getEditorArea();
    layout.setEditorAreaVisible( false );

    final IFolderLayout leftTop = layout.createFolder( "leftTop", IPageLayout.LEFT, 0.3f,// 0.2f,
    editorArea );

    // with left middle and bottom
    // IFolderLayout leftMiddle =
    // layout.createFolder(
    // "leftMiddle",
    // IPageLayout.BOTTOM,
    // 0.4f,
    // "leftTop");
    //		 

    // only leftbottom
    final IFolderLayout leftBottom = layout.createFolder( "leftBottom", IPageLayout.BOTTOM, 0.7f, "leftTop" );

    // IFolderLayout bottom =
    // layout.createFolder(
    // "bottom",
    // IPageLayout.BOTTOM,
    // 0.80f,
    // editorArea);
    final IFolderLayout rightTop = layout.createFolder( "rightTop", IPageLayout.RIGHT, 1.0f, editorArea );
    
    final IPlaceholderFolderLayout veryRight = layout.createPlaceholderFolder( "veryRight", IPageLayout.RIGHT, 0.7f, "rightTop" );

    // IFolderLayout rightMiddle = layout.createFolder( "rightMiddle", IPageLayout.BOTTOM, 0.5f, "rightTop" );

    
    leftTop.addView( WorkflowView.ID );// IPageLayout.ID_RES_NAV);
    // leftMiddle.addView(TasksView.ID);
    // leftBottom.addView(IPageLayout.ID_OUTLINE);
    // bottom.addView(IPageLayout.ID_PROBLEM_VIEW);
    // bottom.addView(RoughnessClsCollectionView.ID);
    // bottom.addView(IPageLayout.ID_)
    // rightTop.addView(SimulationModelDBView.ID);
    // rightTop.addView(IPageLayout.ID_RES_NAV);
    // rightMiddle.addView(ActivitiesView.ID);
    leftBottom.addView( SimulationModelDBView.ID );
    leftBottom.addView( GisMapOutlineView.ID );    
    rightTop.addPlaceholder( MapView.ID );
    rightTop.addPlaceholder( FeatureTemplateView.ID );
    veryRight.addPlaceholder( ActionOptionsView.ID );    
    layout.getViewLayout( FeatureTemplateView.ID ).setCloseable( false );
    layout.getViewLayout( ActionOptionsView.ID ).setCloseable( false );
    layout.getViewLayout( WorkflowView.ID ).setCloseable( false );
    layout.getViewLayout( SimulationModelDBView.ID ).setCloseable( false );
    // TODO: secondary id does not work here: gives assertion failed
//    layout.getViewLayout( MapView.ID + ":*").setCloseable( false );
    layout.getViewLayout( MapView.ID ).setCloseable( false );
    layout.addNewWizardShortcut( Kalypso1D2DNewProjectWizard.ID );
  }
}
