package org.kalypso.editor.mapeditor;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;
import org.kalypso.xml.types.GisviewLayerType;
import org.kalypso.xml.types.LayerType;

/**
 * <p>LabelProvider für Mapview.</p>
 * <p>Geht davon aus, dass die Elemente vom Typ LayerType sind.</p>
 * 
 * 
 * @author gernot
 */
public class LayerlistLabelProvider implements ILabelProvider
{
  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
   */
  public void addListener( ILabelProviderListener listener )
  {
    // kein statechange -> kein listener
  } 

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
   */
  public void dispose()
  {
    // nix zu disposen
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object, java.lang.String)
   */
  public boolean isLabelProperty( Object element, String property )
  {
    return false;
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
   */
  public void removeListener( ILabelProviderListener listener )
  {
    // kein statechange -> kein listener
  }

  /**
   * @see org.eclipse.jface.viewers.ILabelProvider#getImage(java.lang.Object)
   */
  public Image getImage( Object element )
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ILabelProvider#getText(java.lang.Object)
   */
  public String getText( Object element )
  {
    if( !(element instanceof GisviewLayerType) )
        throw new IllegalArgumentException( "element is not of type " + LayerType.class.getName() + ": " + element.getClass().getName() );
    
    return ((GisviewLayerType)element).getName();
  }

}
