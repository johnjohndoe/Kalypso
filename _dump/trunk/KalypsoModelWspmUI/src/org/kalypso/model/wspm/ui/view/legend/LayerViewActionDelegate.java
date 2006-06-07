/**
 * 
 */
package org.kalypso.model.wspm.ui.view.legend;

import org.eclipse.jface.action.IAction;

/**
 * @author Belger
 */
public class LayerViewActionDelegate extends AbstractLegendViewActionDelegate
{
  public void run( final IAction action )
  {
    getView().showLayerProperties();
  }
}
