package org.kalypso.editor.tableeditor;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.part.EditorActionBarContributor;

/**
 * @author belger
 */
public class GisTableEditorActionBarContributor extends EditorActionBarContributor
{
  /**
   * @see org.eclipse.ui.IEditorActionBarContributor#setActiveEditor(org.eclipse.ui.IEditorPart)
   */
  public void setActiveEditor( final IEditorPart targetEditor )
  {
    if( targetEditor != null )
    {
      final IMenuManager menuManager = getActionBars().getMenuManager();
      final IMenuManager tableMenu = menuManager
          .findMenuUsingPath( "org.kalypso.editors.tableeditor.menu" );

      final IMenuManager spaltenMenu = ((GisTableEditor)targetEditor).getLayerTable().getSpaltenMenu(  );
      if( spaltenMenu != null )
        tableMenu.appendToGroup( "spalten", spaltenMenu );
    }
  }
}