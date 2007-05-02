/**
 * 
 */
package org.kalypso.afgui.views;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import de.renew.workflow.base.TaskGroup;

/**
 * @author Stefan Kurzbach
 */
public class WorkflowContentProvider implements ITreeContentProvider
{
  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  public Object[] getChildren( final Object element )
  {
    if( element instanceof TaskGroup )
    {
      final TaskGroup taskGroup = ((TaskGroup) element);
      return taskGroup.getTasks().toArray();
    }    
    else
    {
      return null;
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  public Object getParent( final Object element )
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  public boolean hasChildren( final Object element )
  {
    if( element instanceof TaskGroup )
    {
      final TaskGroup taskGroup = ((TaskGroup) element);
      return !taskGroup.getTasks().isEmpty();
    }
    else
    {
      return false;
    }
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object element )
  {
    return getChildren( element );
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose( )
  {
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {

  }
}