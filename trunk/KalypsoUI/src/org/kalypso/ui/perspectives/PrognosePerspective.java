package org.kalypso.ui.perspectives;

import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.kalypso.ui.IKalypsoUIConstants;

/**
 * @author belger
 */
public class PrognosePerspective implements IPerspectiveFactory
{
  /**
   * @see org.eclipse.ui.IPerspectiveFactory#createInitialLayout(org.eclipse.ui.IPageLayout)
   */
  public void createInitialLayout( final IPageLayout layout )
  {
    layout.addView( IKalypsoUIConstants.ID_PROGNOSE_VIEW, IPageLayout.LEFT, 0.95f, IPageLayout.ID_EDITOR_AREA );
    
    layout.setFixed( true );
    layout.setEditorAreaVisible( false );
  }

}
