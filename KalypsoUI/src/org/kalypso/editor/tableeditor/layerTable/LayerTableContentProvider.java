package org.kalypso.editor.tableeditor.layerTable;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;

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

    final KalypsoFeatureLayer layer = (KalypsoFeatureLayer)((IKalypsoTheme)inputElement).getLayer();
    return layer == null ? new Object[] {} : layer.getAllFeatures();
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
