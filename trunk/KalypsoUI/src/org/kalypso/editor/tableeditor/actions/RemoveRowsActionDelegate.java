package org.kalypso.editor.tableeditor.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.editor.tableeditor.layerTable.LayerTable;
import org.kalypso.editor.tableeditor.layerTable.command.RemoveRowsCommand;
import org.kalypso.java.util.Arrays;
import org.kalypso.ogc.gml.KalypsoFeature;

/**
 * @author bce
 */
public class RemoveRowsActionDelegate extends GisTableAbstractActionDelagate
{
  public void run( final IAction action )
  {
    final LayerTable layerTable = getEditor().getLayerTable();

    final IStructuredSelection selection = (IStructuredSelection)getEditor().getSelection();
    final KalypsoFeature[] features = (KalypsoFeature[])Arrays.castArray( selection.toArray(),
        new KalypsoFeature[selection.size()] );

    getEditor().postCommand( new RemoveRowsCommand( layerTable.getModel(), features ), null );
  }

  /**
   * @see org.kalypso.editor.tableeditor.actions.GisTableAbstractActionDelagate#isEnabled(org.eclipse.jface.viewers.ISelection)
   */
  protected boolean isEnabled( final ISelection selection )
  {
    return !selection.isEmpty();
  }
}