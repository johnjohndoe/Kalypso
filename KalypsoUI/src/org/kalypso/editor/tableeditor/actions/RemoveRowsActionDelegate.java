package org.kalypso.editor.tableeditor.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.editor.tableeditor.layerTable.LayerTable;
import org.kalypso.editor.tableeditor.layerTable.command.RemoveRowsCommand;
import org.kalypso.java.util.Arrays;
import org.kalypso.ogc.sort.DisplayContext;

/**
 * @author bce
 */
public class RemoveRowsActionDelegate extends GisTableAbstractActionDelagate
{
  public void run( final IAction action )
  {
    final LayerTable layerTable = getEditor().getLayerTable();

    final IStructuredSelection selection = (IStructuredSelection)getEditor().getSelection();
    final DisplayContext[] displayContexts = (DisplayContext[])Arrays.castArray( selection.toArray(),
        new DisplayContext[selection.size()] );

    getEditor().postCommand( new RemoveRowsCommand( layerTable.getModel(), displayContexts ), null );
  }

  /**
   * @see org.kalypso.editor.tableeditor.actions.GisTableAbstractActionDelagate#isEnabled(org.eclipse.jface.viewers.ISelection)
   */
  protected boolean isEnabled( final ISelection selection )
  {
    return !selection.isEmpty();
  }
}