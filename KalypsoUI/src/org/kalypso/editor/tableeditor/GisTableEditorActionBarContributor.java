package org.kalypso.editor.tableeditor;

import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.part.EditorActionBarContributor;
import org.kalypso.editor.tableeditor.layerTable.LayerTable;

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
    final IMenuManager menuManager = getActionBars().getMenuManager();
    final IMenuManager spaltenManager = menuManager.findMenuUsingPath( "org.kalypso.editors.tableeditor.menu/org.kalypso.editors.tableeditor.columnmenu" );
    if( spaltenManager != null )
    {
      final GisTableEditor gisTableEditor = (GisTableEditor)targetEditor;
      final LayerTable layerTable = gisTableEditor.getLayerTable();
      
      final IMenuManager menu = layerTable.getMenu();
      if( menu != null )
      {
        final IContributionItem[] items = menu.getItems();
        for( int i = 0; i < items.length; i++ )
        {
          final IContributionItem item = items[i];
          if( item instanceof ActionContributionItem )
            spaltenManager.appendToGroup( "spalten", ((ActionContributionItem)item).getAction() );
        }
      }
    }
  }
}
