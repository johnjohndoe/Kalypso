/**
 * 
 */
package org.kalypso.afgui.views;

import java.util.List;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.afgui.model.IPhase;
import org.kalypso.afgui.model.ISubTaskGroup;
import org.kalypso.afgui.model.ITask;
import org.kalypso.afgui.model.ITaskGroup;
import org.kalypso.afgui.model.IWorkflow;

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
    if( element instanceof IWorkflow )
    {
      final IWorkflow workflow = ((IWorkflow) element);
      final List<IPhase> phases = workflow.getPhases();
      return phases.toArray();
    }
    else if( element instanceof IPhase )
    {
      final IPhase phase = ((IPhase) element);
      final List<ITaskGroup> taskGroups = phase.getTaskGroups();
      return taskGroups.toArray();
    }
    else if( element instanceof ITaskGroup )
    {
      final ITaskGroup taskGroup = ((ITaskGroup) element);
      final List<ISubTaskGroup> subTaskGroups = taskGroup.getSubTaskGroups();
      return subTaskGroups.isEmpty() ? taskGroup.getTasks().toArray() : subTaskGroups.toArray();
    }
    else if( element instanceof ISubTaskGroup )
    {
      return ((ISubTaskGroup) element).getTasks().toArray();
    }
    else if( element instanceof ITask )
    {
      return ((ITask) element).getActivities().toArray();
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
    if( element instanceof IWorkflow )
    {
      final IWorkflow workflow = ((IWorkflow) element);
      return !workflow.getPhases().isEmpty();
    }
    else if( element instanceof IPhase )
    {
      final IPhase phase = ((IPhase) element);
      return !phase.getTaskGroups().isEmpty();
    }
    else if( element instanceof ITaskGroup )
    {
      final ITaskGroup taskGroup = ((ITaskGroup) element);
      return !(taskGroup.getSubTaskGroups().isEmpty() && taskGroup.getTasks().isEmpty());
    }
    else if( element instanceof ISubTaskGroup )
    {
      return !((ISubTaskGroup) element).getTasks().isEmpty();
    }
    else if( element instanceof ITask )
    {
      return !((ITask) element).getActivities().isEmpty();
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