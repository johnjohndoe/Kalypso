package org.kalypso.editor.tableeditor.actions;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree_impl.model.feature.FeatureFactory;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.kalypso.editor.tableeditor.layerTable.LayerTable;
import org.kalypso.editor.tableeditor.layerTable.LayerTableModel;
import org.kalypso.editor.tableeditor.layerTable.command.AddRowCommand;

/**
 * @author belger
 */
public class AddRowActionDelagate extends GisTableAbstractActionDelagate
{
  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    final LayerTable layerTable = getEditor().getLayerTable();
    final LayerTableModel model = layerTable.getModel();

    final FeatureType featureType = model.getFeatureType();
    final Object[] properties = new Object[featureType.getProperties().length];

    final Feature feature = FeatureFactory.createFeature( "x", featureType, properties );

    final Runnable r = new Runnable()
    {
      public void run()
      {
        layerTable.selectRow( feature );
      }
    };

    getEditor().postCommand( new AddRowCommand( model, feature ), r );
  }

  /**
   * @see org.kalypso.editor.tableeditor.actions.GisTableAbstractActionDelagate#isEnabled(org.eclipse.jface.viewers.ISelection)
   */
  protected boolean isEnabled( final ISelection selection )
  {
    return true;
  }
}