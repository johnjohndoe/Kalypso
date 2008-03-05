/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.afgui.scenarios;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.xml.bind.JAXBElement;

import org.eclipse.core.commands.Category;
import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.commands.NotHandledException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.views.WorkflowView;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;

import de.renew.workflow.base.EActivityType;
import de.renew.workflow.base.Task;
import de.renew.workflow.base.TaskGroup;
import de.renew.workflow.connector.worklist.ITaskExecutionAuthority;
import de.renew.workflow.connector.worklist.ITaskExecutionListener;
import de.renew.workflow.connector.worklist.ITaskExecutor;
import de.renew.workflow.connector.worklist.TaskExecutionListener;
import de.renew.workflow.contexts.ContextType;
import de.renew.workflow.contexts.IContextHandlerFactory;
import de.renew.workflow.contexts.WorkbenchPartContextType;
import de.renew.workflow.contexts.WorkbenchSiteContext;

/**
 * @author Stefan Kurzbach
 */
public class TaskExecutor implements ITaskExecutor
{
  private Task m_activeTask;

  private final ITaskExecutionAuthority m_authority;

  private final ICommandService m_commandService;

  private final IHandlerService m_handlerService;

  private final IContextHandlerFactory m_contextHandlerFactory;

  private ArrayList<ITaskExecutionListener> m_taskChangeListeners;

  public TaskExecutor( final IContextHandlerFactory contextHandlerFactory, final ITaskExecutionAuthority authority, final ICommandService commandService, final IHandlerService handlerService )
  {
    m_contextHandlerFactory = contextHandlerFactory;
    m_authority = authority;
    m_commandService = commandService;
    m_handlerService = handlerService;
    m_taskChangeListeners = new ArrayList<ITaskExecutionListener>();
  }

  public Task getActiveTask( )
  {
    return m_activeTask;
  }

  /**
   * @see de.renew.workflow.connector.worklist.ITaskExecutor#stopActiveTask()
   */
  public boolean stopActiveTask( )
  {
    if( m_activeTask != null && m_authority.canStopTask( m_activeTask ) )
    {
      final Collection<String> partsToKeep = new ArrayList<String>();
      partsToKeep.add( WorkflowView.ID );
      partsToKeep.add( PerspectiveWatcher.SCENARIO_VIEW_ID );
      final IWorkbench workbench = PlatformUI.getWorkbench();
      PerspectiveWatcher.cleanPerspective( workbench, partsToKeep );

      /* Tell the listeners, that the task was stopped. */
      fireTaskStopped( m_activeTask );

      m_activeTask = null;
    }
    // if the active task is null it must have been stopped
    return m_activeTask == null;
  }

  /**
   * @see de.renew.workflow.connector.ITaskExecutor#execute(de.renew.workflow.base.Task)
   */
  public IStatus execute( final Task task )
  {
    final List<IStatus> statusList = new ArrayList<IStatus>();

    if( m_activeTask != null )
    {
      // if the same task is executed again, but it is asynchronous, or if the task cannot be stopped, don't do anything
      if( (m_activeTask.getType() == EActivityType.ASYNCHRONOUS && m_activeTask == task) || !m_authority.canStopTask( m_activeTask ) )
      {
        // everything is ok
        return Status.OK_STATUS;
      }
    }

    final String name = task.getURI();
    final Command command = getCommand( m_commandService, name, task instanceof TaskGroup ? TaskExecutionListener.CATEGORY_TASKGROUP : TaskExecutionListener.CATEGORY_TASK );

    final ContextType context = task.getContext();
    if( context != null )
    {
      statusList.add( activateContext( context ) );
    }

    // collect the views that were just opened
    final Collection<String> partsToKeep = collectOpenedViews( context );
    partsToKeep.add( WorkflowView.ID );
    partsToKeep.add( PerspectiveWatcher.SCENARIO_VIEW_ID );

    // forks a new job for cleaning perspective
    final IWorkbench workbench = PlatformUI.getWorkbench();
    PerspectiveWatcher.cleanPerspective( workbench, partsToKeep );

    try
    {
      final Object result = m_handlerService.executeCommand( command.getId(), null );
      if( result instanceof IStatus )
      {
        statusList.add( (IStatus) result );
      }
    }
    catch( final NotHandledException e )
    {
      // this just means that the command is not handled, so there is nothing else to do
    }
    catch( final Throwable e )
    {
      statusList.add( StatusUtilities.statusFromThrowable( e ) );
    }

    m_activeTask = task;

    /* Tell the listeners, that the task was executed. */
    fireTaskExecuted( statusList, task );

    return StatusUtilities.createStatus( statusList, "Beim Ausführen der Aktivität sind Fehler aufgetreten." );
  }

  private Collection<String> collectOpenedViews( final ContextType context )
  {
    if( context == null )
    {
      return new ArrayList<String>();
    }
    final ContextType cause = context.getParent();
    final Collection<String> result;
    if( cause != null )
    {
      result = collectOpenedViews( cause );
    }
    else
    {
      result = new ArrayList<String>();
    }
    if( context instanceof WorkbenchSiteContext )
    {
      final WorkbenchSiteContext multiContext = (WorkbenchSiteContext) context;
      final List<JAXBElement< ? extends WorkbenchPartContextType>> subContexts = multiContext.getPartContexts();
      for( final JAXBElement< ? extends WorkbenchPartContextType> element : subContexts )
      {
        final ContextType value = element.getValue();
        if( value instanceof WorkbenchPartContextType )
          result.add( ((WorkbenchPartContextType) value).getPartId() );
      }
    }
    else if( context instanceof WorkbenchPartContextType )
      result.add( ((WorkbenchPartContextType) context).getPartId() );
    return result;
  }

  /**
   * This function activates the context, which is given and all its parents.
   * 
   * @param context
   *            The context.
   * @return A status object indicating the success of the function.
   */
  private IStatus activateContext( final ContextType context )
  {
    final ContextType parentContext = context.getParent();
    if( parentContext != null )
    {
      // first activate all parent contexts (loop)
      final List<IStatus> statusList = new ArrayList<IStatus>();
      final IStatus activateContext = activateContext( parentContext );
      statusList.add( activateContext );

      if( !activateContext.isOK() )
        return StatusUtilities.createStatus( statusList, "Bei der Aktivierung des Arbeitskontexts sind Fehler aufgetreten." );

      return internalActivateContext( context );
    }

    // loop termination
    return internalActivateContext( context );
  }

  /**
   * This function activates the context, which is given.
   * 
   * @param context
   *            The context, which should be activated.
   * @return A status object indicating the success of the function.
   */
  private IStatus internalActivateContext( final ContextType context )
  {
    // then execute the associated handler
    final IHandler handler = m_contextHandlerFactory.getHandler( context );
    final Command contextCommand = getCommand( m_commandService, context.getId(), TaskExecutionListener.CATEGORY_CONTEXT );
    contextCommand.setHandler( handler );

    try
    {
      final Object result = m_handlerService.executeCommand( context.getId(), null );

      // if result is a status, return it
      if( result instanceof IStatus )
        return (IStatus) result;

      // otherwise everything must be ok
      return Status.OK_STATUS;
    }
    catch( final Throwable e )
    {
      return StatusUtilities.statusFromThrowable( e );
    }
  }

  private static Command getCommand( final ICommandService commandService, final String commandId, final String categoryId )
  {
    final Command command = commandService.getCommand( commandId );
    if( !command.isDefined() )
    {
      final Category category = commandService.getCategory( categoryId );
      if( !category.isDefined() )
      {
        category.define( categoryId, null );
      }
      command.define( commandId, null, category );
    }
    return command;
  }

  /**
   * @see de.renew.workflow.connector.worklist.ITaskExecutor#addTaskChangeListener(de.renew.workflow.connector.worklist.ITaskChangeListener)
   */
  public void addTaskExecutionListener( ITaskExecutionListener listener )
  {
    m_taskChangeListeners.add( listener );
  }

  /**
   * @see de.renew.workflow.connector.worklist.ITaskExecutor#removeTaskChangeListener(de.renew.workflow.connector.worklist.ITaskChangeListener)
   */
  public void removeTaskExecutionListener( ITaskExecutionListener listener )
  {
    m_taskChangeListeners.remove( listener );
  }

  /**
   * This function notifies all registered listeners.
   * 
   * @param results
   *            The results of the task, which was activated, as well of all of its associated tasks.
   * @param task
   *            The final task, that was activated.
   */
  private void fireTaskExecuted( List<IStatus> results, Task task )
  {
    for( int i = 0; i < m_taskChangeListeners.size(); i++ )
    {
      ITaskExecutionListener listener = m_taskChangeListeners.get( i );
      listener.handleTaskExecuted( results, task );
    }
  }

  /**
   * This function notifies all registered listeners.
   * 
   * @param task
   *            The final task, that was activated.
   */
  private void fireTaskStopped( Task task )
  {
    for( int i = 0; i < m_taskChangeListeners.size(); i++ )
    {
      ITaskExecutionListener listener = m_taskChangeListeners.get( i );
      listener.handleTaskStopped( task );
    }
  }
}