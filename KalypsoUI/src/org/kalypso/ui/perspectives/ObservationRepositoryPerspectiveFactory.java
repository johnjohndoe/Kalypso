package org.kalypso.ui.perspectives;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.kalypso.ui.IKalypsoUIConstants;

/**
 * The PerspectiveFactory for browsing the ObservationRepository.
 * 
 * @author schlienger
 */
public class ObservationRepositoryPerspectiveFactory implements IPerspectiveFactory
{
  /**
   * @see IPerspectiveFactory#createInitialLayout(IPageLayout)
   */
  public void createInitialLayout( final IPageLayout layout )
  {
    final IFolderLayout topLeft = layout.createFolder( "topLeft", IPageLayout.LEFT, (float)0.26,
        layout.getEditorArea() );
    topLeft.addView( IKalypsoUIConstants.ID_REPOSITORY_VIEW );
    topLeft.addPlaceholder( IPageLayout.ID_RES_NAV );

    final IFolderLayout botLeft = layout.createFolder( "bottom", IPageLayout.BOTTOM, (float)0.70,
        "topLeft" );
    botLeft.addView( IPageLayout.ID_PROP_SHEET );

    final IFolderLayout leftBottom = layout.createFolder( "leftBottom", IPageLayout.BOTTOM,
        (float)0.60, layout.getEditorArea() );
    leftBottom.addView( IKalypsoUIConstants.ID_OBSDIAGRAM_VIEW );

    final IFolderLayout rightBottom = layout.createFolder( "rightBottom", IPageLayout.RIGHT,
        (float)0.50, "leftBottom" );
    rightBottom.addView( IKalypsoUIConstants.ID_OBSTABLE_VIEW );

    setContentsOfShowViewMenu( layout );
  }

  /**
   * Sets the intial contents of the "Show View" menu.
   */
  protected void setContentsOfShowViewMenu( IPageLayout layout )
  {
    layout.addShowViewShortcut( IPageLayout.ID_OUTLINE );
    layout.addShowViewShortcut( IPageLayout.ID_RES_NAV );
    layout.addShowViewShortcut( IKalypsoUIConstants.ID_OBSDIAGRAM_VIEW );
    layout.addShowViewShortcut( IKalypsoUIConstants.ID_OBSTABLE_VIEW );
  }
}