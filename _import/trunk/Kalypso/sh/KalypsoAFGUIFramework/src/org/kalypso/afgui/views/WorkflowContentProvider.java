/**
 * 
 */
package org.kalypso.afgui.views;

import java.util.List;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.afgui.workflow.Phase;
import org.kalypso.afgui.workflow.Task;
import org.kalypso.afgui.workflow.TaskGroup;
import org.kalypso.afgui.workflow.Workflow;

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
    if( element instanceof Workflow )
    {
      final Workflow workflow = ((Workflow) element);
      final List<Phase> phases = workflow.getPhases();
      return phases.toArray();
    }
    else if( element instanceof Phase )
    {
      final Phase phase = ((Phase) element);
      final List<TaskGroup> taskGroups = phase.getTaskGroups();
      return taskGroups.toArray();
    }
    else if( element instanceof TaskGroup )
    {
      final TaskGroup taskGroup = ((TaskGroup) element);
      return taskGroup.getTasks().toArray();
    }
    else if( element instanceof Task )
    {
      return ((Task) element).getActivities().toArray();
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
    if( element instanceof Workflow )
    {
      final Workflow workflow = ((Workflow) element);
      return !workflow.getPhases().isEmpty();
    }
    else if( element instanceof Phase )
    {
      final Phase phase = ((Phase) element);
      return !phase.getTaskGroups().isEmpty();
    }
    else if( element instanceof TaskGroup )
    {
      final TaskGroup taskGroup = ((TaskGroup) element);
      return !taskGroup.getTasks().isEmpty();
    }
    else if( element instanceof Task )
    {
      return !((Task) element).getActivities().isEmpty();
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