package org.kalypso.ui.editor.gistableeditor.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;

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
//    final LayerTableViewer layerTable = getEditor().getLayerTable();
//
//    final IKalypsoTheme theme = layerTable.getTheme();
//    
//    final FeatureType featureType = layer.getFeatureType();
//
//    final Feature feature =FeatureFactory.createFeature("x",featureType); 
//
//    final Runnable r = new Runnable()
//    {
//      public void run()
//      {
//        layerTable.selectRow( feature );
//      }
//    };
//
//    final ICommand command = new AddFeatureCommand( layer, feature );
//    layerTable.postCommand( command, r );
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