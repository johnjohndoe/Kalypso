package org.kalypso.ui.editor.gmleditor.ui;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.ui.editor.gmleditor.util.model.IModel;

public class GMLEditorContentProvider implements ITreeContentProvider
{
  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose()
  {/**/}

  public void inputChanged( Viewer m_viewer, Object oldInput, Object newInput )
  {
  // nichts tun
  }

  /**
   * @see ITreeContentProvider#getChildren(Object)
   */
  public Object[] getChildren( Object parentElement )
  {
    return ( (IModel)parentElement ).getChildren();
  }

  /**
   * @see ITreeContentProvider#getParent(Object)
   */
  public Object getParent( Object element )
  {
    return ( (IModel)element ).getParent();
  }

  /**
   * @see ITreeContentProvider#hasChildren(Object)
   */
  public boolean hasChildren( Object element )
  {
    return ( (IModel)element ).hasChildren();
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( Object inputElement )
  {
    return getChildren( inputElement );
  }
}