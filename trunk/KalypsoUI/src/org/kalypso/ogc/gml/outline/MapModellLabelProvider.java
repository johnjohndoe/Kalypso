package org.kalypso.ogc.gml.outline;

import org.deegree.graphics.sld.UserStyle;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;
import org.kalypso.ogc.gml.IKalypsoTheme;

/**
 * @author bce
 */
public class MapModellLabelProvider implements ILabelProvider
{
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
    if( element instanceof IKalypsoTheme )
      return ((IKalypsoTheme)element).getName();
    
    if( element instanceof ThemeStyleTreeObject )
        return element.toString();
    
    if( element instanceof UserStyle )
      return ((UserStyle)element).getName();
    
    return element.toString();
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
   */
  public void addListener( ILabelProviderListener listener )
  {
  // unsused  
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
   */
  public void dispose()
  {
  // unused  
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
  // unused  
  }
}
