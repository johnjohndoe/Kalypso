package org.kalypso.kalypso1d2d.pjt.perspective;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.eclipse.ui.IPlaceholderFolderLayout;
import org.kalypso.featureview.views.FeatureView;
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

    final IFolderLayout leftTop = layout.createFolder( "leftTop", IPageLayout.LEFT, 0.3f, editorArea );
    final IFolderLayout leftBottom = layout.createFolder( "leftBottom", IPageLayout.BOTTOM, 0.7f, "leftTop" );
    final IFolderLayout rightTop = layout.createFolder( "rightTop", IPageLayout.RIGHT, 1.0f, editorArea );
    final IPlaceholderFolderLayout rightBottom = layout.createPlaceholderFolder( "rightBottom", IPageLayout.BOTTOM, 0.7f, "rightTop" );
    final IPlaceholderFolderLayout veryRight = layout.createPlaceholderFolder( "veryRight", IPageLayout.RIGHT, 0.7f, "rightTop" );

    leftTop.addView( WorkflowView.ID );
    leftBottom.addView( SimulationModelDBView.ID );
    leftBottom.addView( GisMapOutlineView.ID );
    rightTop.addPlaceholder( MapView.ID );
    rightTop.addPlaceholder( FeatureTemplateView.ID );
    rightBottom.addPlaceholder( FeatureView.ID );
    veryRight.addPlaceholder( ActionOptionsView.ID );

    layout.getViewLayout( FeatureView.ID ).setMoveable( false );
    layout.getViewLayout( FeatureTemplateView.ID ).setCloseable( false );
    layout.getViewLayout( FeatureTemplateView.ID ).setMoveable( false );
    layout.getViewLayout( ActionOptionsView.ID ).setCloseable( false );
    layout.getViewLayout( ActionOptionsView.ID ).setMoveable( false );
    layout.getViewLayout( WorkflowView.ID ).setCloseable( false );
    layout.getViewLayout( WorkflowView.ID ).setMoveable( false );
    layout.getViewLayout( SimulationModelDBView.ID ).setCloseable( false );
    layout.getViewLayout( SimulationModelDBView.ID ).setMoveable( false );
    // TODO: secondary id does not work here: gives assertion failed
    // layout.getViewLayout( MapView.ID + ":*").setCloseable( false );
    layout.getViewLayout( MapView.ID ).setCloseable( false );
    layout.getViewLayout( MapView.ID ).setMoveable( false );
    layout.addNewWizardShortcut( Kalypso1D2DNewProjectWizard.ID );
  }
}
