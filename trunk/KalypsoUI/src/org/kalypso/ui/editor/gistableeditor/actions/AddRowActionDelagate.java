package org.kalypso.ui.editor.gistableeditor.actions;

import org.deegree.model.feature.FeatureType;
import org.deegree_impl.model.feature.FeatureFactory;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.command.AddFeatureCommand;
import org.kalypso.ogc.gml.table.LayerTableViewer;
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
    final LayerTableViewer layerTable = getEditor().getLayerTable();

    final IKalypsoTheme theme = layerTable.getTheme();
    
    KalypsoFeatureLayer layer = (KalypsoFeatureLayer)theme.getLayer();
    final FeatureType featureType = layer.getFeatureType();
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

    final ICommand command = new AddFeatureCommand( layer, feature );
    layerTable.postCommand( command, r );
  }

  /**
   * @see org.kalypso.ui.editor.gistableeditor.actions.GisTableAbstractActionDelagate#isEnabled(org.eclipse.jface.viewers.ISelection)
   */
  protected boolean isEnabled( final ISelection selection )
  {
    return getEditor().getLayerTable().getTheme() != null;
  }

  /**
   * @see org.kalypso.ui.editor.gistableeditor.actions.GisTableAbstractActionDelagate#isChecked()
   */
  protected boolean isChecked()
  {
    return false;
  }
}