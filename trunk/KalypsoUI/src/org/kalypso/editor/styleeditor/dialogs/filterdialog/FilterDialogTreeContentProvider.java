package org.kalypso.editor.styleeditor.dialogs.filterdialog;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

public class FilterDialogTreeContentProvider implements ITreeContentProvider
{
  protected Viewer m_viewer = null;

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  public Object[] getChildren(final Object parentElement)
  {
  	return  ((FilterDialogTreeNode)parentElement).getChildren();
//  	
//  	 Object[] object = null;
//  	 
//  	return  ((FilterDialogTreeNode)parentElement).getChildren();
//
//     if (parentElement != null) {
//         Object[] nodeList = ((FilterDialogTreeNode)parentElement).getChildren();
//         Object[] tempArray = new Object[nodeList.length];
//         int j = 0;
//
//         for (int i = 0; i < nodeList.length; i++) {
//           tempArray[j++] = nodeList[i];           
//         }
//
//         object = new Object[j];
//
//         for (int i = 0; i < j; i++) {
//             object[i] = tempArray[i];
//         }
//     } else {
//         object = new Object[0];
//     }
//
//     return object;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  public Object getParent(final Object element)
  {
    return null;
  }
 
  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  public boolean hasChildren(final Object element)
  {
    Object[] children = getChildren(element);
    return (children != null);
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements(final Object inputElement)
  {
    return getChildren(inputElement);
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose()
  {
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer,
   *      java.lang.Object, java.lang.Object)
   */
  public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput)
  {   
  }
}