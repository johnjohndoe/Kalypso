package org.kalypso.ogc.gml.table;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

/**
 * @author bce
 */
public class LayerTableContentProvider implements IStructuredContentProvider
{
  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object inputElement )
  {
    if( inputElement == null )
      return new Object[] {};

    return null;
    
    // TODO: change input to what?
//    final GMLWorkspace theme = (KalypsoFeatureTheme)inputElement;
//    
//    return theme == null ? new Object[] {} : theme.getWork.getAllFeatures();
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose()
  {
  //  
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object, java.lang.Object)
   */
  public void inputChanged( final Viewer viewer, Object oldInput, Object newInput )
  {
    //
  }

}
