package org.kalypso.model.wspm.ui.product;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.eclipse.ui.IPlaceholderFolderLayout;
import org.kalypso.model.wspm.ui.view.LayerView;
import org.kalypso.model.wspm.ui.view.legend.LegendView;
import org.kalypso.model.wspm.ui.view.table.TableView;



/**
 * @author Gernot Belger
 */
public class ProfileditorPerspective implements IPerspectiveFactory
{
  public final static String ID = "org.kalypso.model.wspm.ui.product.ProfileditorPerspective"; //$NON-NLS-1$
  
  /**
   * @see org.eclipse.ui.IPerspectiveFactory#createInitialLayout(org.eclipse.ui.IPageLayout)
   */
  public void createInitialLayout( final IPageLayout layout )
  {
    layout.setEditorAreaVisible( true );

    final IFolderLayout leftFolder = layout.createFolder( "left", IPageLayout.LEFT, 0.4f, IPageLayout.ID_EDITOR_AREA ); //$NON-NLS-1$
    leftFolder.addView( LegendView.class.getName() );
    layout.addView( LayerView.class.getName(), IPageLayout.BOTTOM, 0.4f, "left" ); //$NON-NLS-1$
    
    final IPlaceholderFolderLayout topTable = layout.createPlaceholderFolder( "tableview", IPageLayout.TOP, 0.5f, IPageLayout.ID_EDITOR_AREA ); //$NON-NLS-1$
    topTable.addPlaceholder( TableView.class.getName() );
    topTable.addPlaceholder( TableView.class.getName() + ":*" ); //$NON-NLS-1$
    
    layout.addShowViewShortcut( LegendView.class.getName() );
    layout.addShowViewShortcut( LayerView.class.getName() );
    layout.addShowViewShortcut( TableView.class.getName() );
    layout.addShowViewShortcut( IPageLayout.ID_PROBLEM_VIEW );
  }
}
