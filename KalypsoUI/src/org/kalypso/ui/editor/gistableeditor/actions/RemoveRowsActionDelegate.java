package org.kalypso.ui.editor.gistableeditor.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;

/**
 * @author bce
 */
public class RemoveRowsActionDelegate extends GisTableAbstractActionDelagate
{
  public void run( final IAction action )
  {
//    final GisTableEditor editor = getEditor();
//    final LayerTableViewer layerTable = editor.getLayerTable();
//
//    final IStructuredSelection selection = (IStructuredSelection)editor.getSelection();
//    final Feature[] features = (Feature[])Arrays.castArray( selection.toArray(),
//        new Feature[selection.size()] );
//
//    final ICommand command = new RemoveFeaturesCommand( layer, features );
//    editor.getLayerTable().getTheme().postCommand( command, null );
  }

  /**
   * @see org.kalypso.ui.editor.gistableeditor.actions.GisTableAbstractActionDelagate#isEnabled(org.eclipse.jface.viewers.ISelection)
   */
  protected boolean isEnabled( final ISelection selection )
  {
    return getEditor().getLayerTable().getTheme() != null && !selection.isEmpty();
  }

  /**
   * @see org.kalypso.ui.editor.gistableeditor.actions.GisTableAbstractActionDelagate#isChecked()
   */
  protected boolean isChecked()
  {
    return false;
  }
}