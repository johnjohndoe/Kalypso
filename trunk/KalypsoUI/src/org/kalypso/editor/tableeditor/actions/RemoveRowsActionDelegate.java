package org.kalypso.editor.tableeditor.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.editor.tableeditor.GisTableEditor;
import org.kalypso.editor.tableeditor.layerTable.LayerTable;
import org.kalypso.editor.tableeditor.layerTable.command.RemoveRowsCommand;
import org.kalypso.java.util.Arrays;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.util.command.ICommand;

/**
 * @author bce
 */
public class RemoveRowsActionDelegate extends GisTableAbstractActionDelagate
{
  public void run( final IAction action )
  {
    final GisTableEditor editor = getEditor();
    final LayerTable layerTable = editor.getLayerTable();

    final IStructuredSelection selection = (IStructuredSelection)editor.getSelection();
    final KalypsoFeature[] features = (KalypsoFeature[])Arrays.castArray( selection.toArray(),
        new KalypsoFeature[selection.size()] );

    final ICommand command = new RemoveRowsCommand( layerTable.getModel(), features );
    editor.getTheme().postCommand( command, null );
  }

  /**
   * @see org.kalypso.editor.tableeditor.actions.GisTableAbstractActionDelagate#isEnabled(org.eclipse.jface.viewers.ISelection)
   */
  protected boolean isEnabled( final ISelection selection )
  {
    return getEditor().getTheme() != null && !selection.isEmpty();
  }

  /**
   * @see org.kalypso.editor.tableeditor.actions.GisTableAbstractActionDelagate#isChecked()
   */
  protected boolean isChecked()
  {
    return false;
  }
}