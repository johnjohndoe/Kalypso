package org.kalypso.editor.tableeditor.actions;

import org.deegree.model.feature.FeatureType;
import org.deegree_impl.model.feature.FeatureFactory;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.kalypso.editor.tableeditor.layerTable.LayerTable;
import org.kalypso.editor.tableeditor.layerTable.LayerTableModel;
import org.kalypso.editor.tableeditor.layerTable.command.AddRowCommand;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.util.command.CommandJob;
import org.kalypso.util.command.ICommand;

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

    final KalypsoFeature feature = new KalypsoFeature( FeatureFactory.createFeature( "x",
        featureType, properties ) );

    final Runnable r = new Runnable()
    {
      public void run()
      {
        layerTable.selectRow( feature );
      }
    };

    final ICommand command = new AddRowCommand( model, feature );
    new CommandJob( command, getEditor().getLayerCommandManager(), getEditor().getSchedulingRule(),
        r, CommandJob.POST );
  }

  /**
   * @see org.kalypso.editor.tableeditor.actions.GisTableAbstractActionDelagate#isEnabled(org.eclipse.jface.viewers.ISelection)
   */
  protected boolean isEnabled( final ISelection selection )
  {
    return true;
  }

  /**
   * @see org.kalypso.editor.tableeditor.actions.GisTableAbstractActionDelagate#isChecked()
   */
  protected boolean isChecked()
  {
    return false;
  }
}