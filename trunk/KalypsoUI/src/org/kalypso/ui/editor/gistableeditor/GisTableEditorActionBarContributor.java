package org.kalypso.ui.editor.gistableeditor;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.part.EditorActionBarContributor;

/**
 * @author belger
 */
public class GisTableEditorActionBarContributor extends EditorActionBarContributor
{
  private static final String M_SPALTEN = "spaltenSubMenu";

  /**
   * @see org.eclipse.ui.IEditorActionBarContributor#setActiveEditor(org.eclipse.ui.IEditorPart)
   */
  public void setActiveEditor( final IEditorPart targetEditor )
  {
    if( targetEditor != null )
    {
      final IMenuManager menuManager = getActionBars().getMenuManager();
      final IMenuManager tableMenu = menuManager
          .findMenuUsingPath( "org.kalypso.ui.editors.tableeditor.menu" );
      if( tableMenu != null && tableMenu.find( M_SPALTEN ) == null )
      {
        final IMenuManager spaltenMenu = ((GisTableEditor)targetEditor).createSpaltenMenu( M_SPALTEN );
        spaltenMenu.add( new Action( "Hallo" ) { /* dummy item, damit das Menu überhaupt eingebaut wird */} );
        tableMenu.appendToGroup( "spalten", spaltenMenu );
      }
    }
  }
}