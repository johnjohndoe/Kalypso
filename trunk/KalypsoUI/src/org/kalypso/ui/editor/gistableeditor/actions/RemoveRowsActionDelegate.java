package org.kalypso.ui.editor.gistableeditor.actions;

import org.deegree.model.feature.Feature;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.java.util.Arrays;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.command.RemoveFeaturesCommand;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ui.editor.gistableeditor.GisTableEditor;
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
    final Feature[] features = (Feature[])Arrays.castArray( selection.toArray(),
        new Feature[selection.size()] );

    KalypsoFeatureLayer layer = (KalypsoFeatureLayer)layerTable.getTheme().getLayer();
    final ICommand command = new RemoveFeaturesCommand( layer, features );
    editor.getLayerTable().getTheme().postCommand( command, null );
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