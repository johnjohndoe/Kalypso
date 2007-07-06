package org.kalypso.kalypso1d2d.pjt.perspective;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.eclipse.ui.IPlaceholderFolderLayout;
import org.eclipse.ui.internal.PageLayout;
import org.kalypso.featureview.views.FeatureView;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DNewProjectWizard;
import org.kalypso.kalypso1d2d.pjt.views.WorkflowView;
import org.kalypso.model.wspm.ui.view.chart.ChartView;
import org.kalypso.ogc.gml.outline.GisMapOutlineView;
import org.kalypso.ogc.sensor.view.DiagramViewPart;
import org.kalypso.ogc.sensor.view.TableViewPart;
import org.kalypso.ui.editor.featureeditor.FeatureTemplateView;
import org.kalypso.ui.editor.mapeditor.views.ActionOptionsView;
import org.kalypso.ui.repository.view.RepositoryExplorerPart;
import org.kalypso.ui.views.map.MapView;

// TODO: move the perspective to the KalypsoModelProduct
@SuppressWarnings("restriction")
public class Perspective implements IPerspectiveFactory
{
  final static public String ID = "org.kalypso.kalypso1d2d.pjt.perspective.Perspective";

  public static final String SCENARIO_VIEW_ID = "org.kalypso.kalypso1d2d.pjt.views.ScenarioView";

  public void createInitialLayout( final IPageLayout layout )
  {
    // HACK: make sure that all action sets are initially disabled for this perspective
    final PageLayout pl = (PageLayout) layout;
    pl.getActionSets().clear();

    // Get the editor area.
    final String editorArea = layout.getEditorArea();
    layout.setEditorAreaVisible( false );

    final IFolderLayout leftTop = layout.createFolder( "leftTop", IPageLayout.LEFT, 0.3f, editorArea );
    final IFolderLayout leftBottom = layout.createFolder( "leftBottom", IPageLayout.BOTTOM, 0.7f, "leftTop" );
    final IFolderLayout rightTop = layout.createFolder( "rightTop", IPageLayout.RIGHT, 1.0f, editorArea );
    final IPlaceholderFolderLayout rightBottom = layout.createPlaceholderFolder( "rightBottom", IPageLayout.BOTTOM, 0.7f, "rightTop" );
    final IPlaceholderFolderLayout veryRight = layout.createPlaceholderFolder( "veryRight", IPageLayout.RIGHT, 0.7f, "rightTop" );

    leftTop.addView( WorkflowView.ID );
    leftBottom.addView( SCENARIO_VIEW_ID );
    
    leftBottom.addPlaceholder( GisMapOutlineView.ID );
    leftBottom.addPlaceholder( RepositoryExplorerPart.ID );

    rightTop.addPlaceholder( MapView.ID );
    rightTop.addPlaceholder( FeatureTemplateView.ID );
    rightTop.addPlaceholder( DiagramViewPart.ID );

    rightBottom.addPlaceholder( FeatureView.ID );
    rightBottom.addPlaceholder( ChartView.ID );
    rightBottom.addPlaceholder( TableViewPart.ID );

    veryRight.addPlaceholder( ActionOptionsView.ID );

    /* Moveability and closeability of the views. */
    layout.getViewLayout( FeatureView.ID ).setMoveable( true );
    layout.getViewLayout( FeatureTemplateView.ID ).setCloseable( false );
    layout.getViewLayout( FeatureTemplateView.ID ).setMoveable( false );
    layout.getViewLayout( ActionOptionsView.ID ).setCloseable( false );
    layout.getViewLayout( ActionOptionsView.ID ).setMoveable( false );
    layout.getViewLayout( WorkflowView.ID ).setCloseable( false );
    layout.getViewLayout( WorkflowView.ID ).setMoveable( false );
    layout.getViewLayout( SCENARIO_VIEW_ID ).setCloseable( false );
    layout.getViewLayout( SCENARIO_VIEW_ID ).setMoveable( false );
    layout.getViewLayout( RepositoryExplorerPart.ID ).setCloseable( false );
    layout.getViewLayout( RepositoryExplorerPart.ID ).setMoveable( false );
    layout.getViewLayout( DiagramViewPart.ID ).setCloseable( false );
    layout.getViewLayout( DiagramViewPart.ID ).setMoveable( false );
    layout.getViewLayout( TableViewPart.ID ).setCloseable( false );
    layout.getViewLayout( TableViewPart.ID ).setMoveable( false );
    // TODO: secondary id does not work here: gives assertion failed
    // layout.getViewLayout( MapView.ID + ":*").setCloseable( false );
    layout.getViewLayout( MapView.ID ).setCloseable( false );
    layout.getViewLayout( MapView.ID ).setMoveable( false );
    layout.addNewWizardShortcut( Kalypso1D2DNewProjectWizard.ID );
  }
}
