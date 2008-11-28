/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.afgui;

import java.util.List;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.afgui.scenarios.ScenarioHelper;

import de.renew.workflow.base.ITask;
import de.renew.workflow.base.ITaskGroup;
import de.renew.workflow.base.IWorkflow;
import de.renew.workflow.connector.WorkflowProjectNature;
import de.renew.workflow.connector.worklist.ITaskExecutor;
import de.renew.workflow.contexts.WorkflowSystemExtension;

/**
 * A helper for the workflow.
 *
 * @author Holger Albert
 *
 */
public class WorkflowHelper
{
  /**
   * The constructor.
   */
  private WorkflowHelper( )
  {
  }

  /**
   * This function returns the project of the current active scenario. Null, if no scenario is active.
   *
   * @return The project of the current active scenario.
   */
  public static IProject getProject( )
  {
    final IFolder scenarioFolder = ScenarioHelper.getScenarioFolder();
    if( scenarioFolder == null )
      return null;

    return scenarioFolder.getProject();
  }

  /**
   * This function returns the tasks of the workflow.
   *
   * @param project
   *            The project.
   *
   * @return The tasks of the workflow.
   */
  public static List<ITask> getWorkflowTasks( final IProject project ) throws CoreException
  {
    if( project == null )
      return null;

    final IWorkflow workflow = getWorkflow( project );
    if( workflow == null )
      return null;

    return workflow.getTasks();
  }

  /**
   * This function returns the workflow.
   *
   * @param project
   *            The project.
   *
   * @return The workflow.
   */
  public static IWorkflow getWorkflow( final IProject project ) throws CoreException
  {
    if( project == null )
      return null;

    /* Get the workflow. */
    final WorkflowProjectNature workflowNature = WorkflowProjectNature.toThisNature( project );
    if( workflowNature == null )
      return null;

    return workflowNature.getCurrentWorklist();
  }

  /**
   * This function returns the tasks of the workflow with the given ID.
   *
   * @param workflow_id
   *            The ID of the workflow.
   *
   * @return The tasks of the workflow.
   */
  public static List<ITask> getWorkflowTasks( final String workflow_id )
  {
    final IWorkflow workflow = getWorkflow( workflow_id );
    if( workflow == null )
      return null;

    return workflow.getTasks();
  }

  /**
   * This function returns the workflow with the given ID.
   *
   * @param workflow_id
   *            The ID of the workflow.
   *
   * @return The workflow.
   */
  public static IWorkflow getWorkflow( final String workflow_id )
  {
    return WorkflowSystemExtension.getWorkflow( workflow_id );
  }

  /**
   * This function returns default task of the workflow.
   *
   * @param workflow
   *            The workflow.
   *
   * @return The default task of the workflow.
   */
  public static ITask getDefaultTask( final IWorkflow workflow )
  {
    if( workflow == null )
      return null;

    return workflow.getDefaultTask();
  }

  /**
   * This function returns the current active task.
   *
   * @return The active task or null, if no task is active.
   */
  public static ITask getActiveTask( )
  {
    /* Get the active task. */
    final ITaskExecutor taskExecutor = KalypsoAFGUIFrameworkPlugin.getDefault().getTaskExecutor();

    return taskExecutor.getActiveTask();
  }

  /**
   * This function activates a task.
   *
   * @param task
   *            The task, which should be activated.
   *
   * @return The result of the execution.
   */
  public static IStatus activateTask( final ITask task )
  {
    final ITaskExecutor taskExecutor = KalypsoAFGUIFrameworkPlugin.getDefault().getTaskExecutor();
    final IStatus result = taskExecutor.execute( task );

    return result;
  }

  /**
   * This function tries to find the task group, which contains the given task. If it can find none, it will return
   * null.
   *
   * @param tasks
   *            All tasks available.
   * @param task
   *            The task, for which the task group should be searched.
   *
   * @return The task group, containing the task or null.
   */
  public static ITaskGroup findTaskGroup( final List<ITask> tasks, final ITask taskToFind )
  {
    for( int i = 0; i < tasks.size(); i++ )
    {
      final ITask task = tasks.get( i );

      if( task instanceof ITaskGroup )
      {
        /* Convert. */
        final ITaskGroup taskGroup = (ITaskGroup) task;

        /* If it is contained in the current task group, return it. */
        if( taskGroup.getTasks().contains( taskToFind ) )
          return taskGroup;

        /* Otherwise, find the task in the task groups of this task group. */
        final ITaskGroup foundTaskGroup = findTaskGroup( taskGroup.getTasks(), taskToFind );

        /* If found, return it. */
        if( foundTaskGroup != null )
          return foundTaskGroup;

        /* If not, check the next task of the current list. */
        continue;
      }
    }

    return null;
  }

  /**
   * This function finds a workflow task by its URI.
   *
   * @param uri
   *            The task URI.
   * @return The task, or null if not found.
   */
  public static ITask findTask( final List<ITask> tasks, final String uri )
  {
    if( tasks == null || tasks.size() == 0 || uri == null )
      return null;

    for( int i = 0; i < tasks.size(); i++ )
    {
      final ITask task = tasks.get( i );

      if( task instanceof ITaskGroup )
      {
        final ITaskGroup taskGroup = (ITaskGroup) task;

        final ITask foundTask = findTask( taskGroup.getTasks(), uri );
        if( foundTask != null )
          return foundTask;

        continue;
      }

      if( task.getURI().equals( uri ) )
        return task;

      continue;
    }

    return null;
  }
}