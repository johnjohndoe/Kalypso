package org.kalypso.editor.tableeditor.layerTable;

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
    return ((LayerTableModel)inputElement).getLayer().getAllDisplayContexts();
    //return ((LayerTableModel)inputElement).getLayer().getAllFeatures();
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
