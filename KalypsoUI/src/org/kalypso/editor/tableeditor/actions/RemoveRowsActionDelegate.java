package org.kalypso.editor.tableeditor.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.editor.tableeditor.GisTableEditor;
import org.kalypso.editor.tableeditor.layerTable.LayerTableViewer;
import org.kalypso.java.util.Arrays;
import org.kalypso.ogc.command.RemoveFeaturesCommand;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.util.command.ICommand;

/**
 * @author bce
 */
public class RemoveRowsActionDelegate extends GisTableAbstractActionDelagate
{
  public void run( final IAction action )
  {
    final GisTableEditor editor = getEditor();
    final LayerTableViewer layerTable = editor.getLayerTable();

    final IStructuredSelection selection = (IStructuredSelection)editor.getSelection();
    final KalypsoFeature[] features = (KalypsoFeature[])Arrays.castArray( selection.toArray(),
        new KalypsoFeature[selection.size()] );

    KalypsoFeatureLayer layer = (KalypsoFeatureLayer)layerTable.getTheme().getLayer();
    final ICommand command = new RemoveFeaturesCommand( layer, features );
    editor.getLayerTable().getTheme().postCommand( command, null );
  }

  /**
   * @see org.kalypso.editor.tableeditor.actions.GisTableAbstractActionDelagate#isEnabled(org.eclipse.jface.viewers.ISelection)
   */
  protected boolean isEnabled( final ISelection selection )
  {
    return getEditor().getLayerTable().getTheme() != null && !selection.isEmpty();
  }

  /**
   * @see org.kalypso.editor.tableeditor.actions.GisTableAbstractActionDelagate#isChecked()
   */
  protected boolean isChecked()
  {
    return false;
  }
}