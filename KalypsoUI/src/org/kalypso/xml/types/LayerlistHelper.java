package org.kalypso.xml.types;

import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.StructuredSelection;

/**
 * @author gernot
 */
public class LayerlistHelper
{
  /**
   * Not intended to be instantiated
   */
  private LayerlistHelper()
  {
    // wird nie aufgerufen
  }
  
  public final static void selectLayer( final List layers, final LayerType layer, final ISelectionProvider selectionProvider )
  {
    for( final Iterator lIt = layers.iterator(); lIt.hasNext(); )
    {
       final LayerType tmpLayer = (LayerType)lIt.next();
       if( tmpLayer.getId().equals( layer.getId() ) )
       {
         selectionProvider.setSelection( new StructuredSelection( layer ) );
         return;
       }
    }
  }

}
