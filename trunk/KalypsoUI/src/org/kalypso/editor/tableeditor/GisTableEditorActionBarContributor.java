package org.kalypso.editor.tableeditor;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.SubMenuManager;
import org.eclipse.ui.part.EditorActionBarContributor;
import org.kalypso.editor.tableeditor.actions.AddRowAction;

/**
 * @author bce
 */
public class GisTableEditorActionBarContributor extends EditorActionBarContributor
{
  private IAction m_addAction;
  
  public GisTableEditorActionBarContributor()
  {
    m_addAction = new AddRowAction( this );
  }
  
  /**
   * @see org.eclipse.ui.part.EditorActionBarContributor#contributeToMenu(org.eclipse.jface.action.IMenuManager)
   */
  public void contributeToMenu( IMenuManager menuManager )
  {
    super.contributeToMenu( menuManager );
    
    final SubMenuManager subMenu = new SubMenuManager( menuManager );

    subMenu.add(m_addAction);
  }
  /**
   * @see org.eclipse.ui.part.EditorActionBarContributor#contributeToToolBar(org.eclipse.jface.action.IToolBarManager)
   */
  public void contributeToToolBar( IToolBarManager toolBarManager )
  {
    super.contributeToToolBar( toolBarManager );
    
    toolBarManager.add(m_addAction);
  }
}
