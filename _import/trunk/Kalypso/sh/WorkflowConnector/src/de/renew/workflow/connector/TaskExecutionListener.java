/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package de.renew.workflow.connector;

import java.util.logging.Logger;

import org.eclipse.core.commands.Category;
import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.commands.NotHandledException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.IHandlerService;

import de.renew.workflow.base.EActivityExeState;
import de.renew.workflow.base.Task;
import de.renew.workflow.cases.CaseData;
import de.renew.workflow.cases.ICaseDataProvider;
import de.renew.workflow.cases.ITaskExecutionListener;
import de.renew.workflow.cases.TaskExecutionException;
import de.renew.workflow.contexts.ContextType;
import de.renew.workflow.contexts.IContextHandlerFactory;

/**
 * A {@link TaskExecutionListener} is a special command handler that handles requesting and confirming work items for
 * commands. This {@link ITaskExecutionListener} requests a work item from the workflow system before the command it is
 * executed and confirms it after its execution has finished successfully and if the activity requires confirmation. If
 * there is a problem in the internal execution or if the command is cancelled, the work item is cancelled.
 * 
 * @author Stefan Kurzbach
 */
public class TaskExecutionListener implements ITaskExecutionListener
{

  public static Logger logger = Logger.getLogger( TaskExecutionListener.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "de.renew.workflow.connector/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  private static final boolean m_isWorkflowMode = false;

  private static final String TASKS_COMMANDS_CATEGORY = "org.kalypso.kalypso1d2d.pjt.TasksCommands"; //$NON-NLS-1$

  private WorkflowConnector m_connector;

  private Task m_activeTask;

  protected final IContextHandlerFactory m_contextHandlerFactory;

  private final ICommandService m_commandService;

  private final IHandlerService m_handlerService;

  private final ICaseDataProvider m_dataProvider;

  private final ITaskExecutionAuthority m_authority;

  public TaskExecutionListener( final IContextHandlerFactory contextHandlerFactory, final ITaskExecutionAuthority authority, final ICaseDataProvider dataProvider )
  {
    m_contextHandlerFactory = contextHandlerFactory;
    m_authority = authority;
    m_dataProvider = dataProvider;
    final IWorkbench workbench = PlatformUI.getWorkbench();
    m_commandService = (ICommandService) workbench.getService( ICommandService.class );
    m_handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
  }

  /**
   * @see de.renew.workflow.cases.ITaskExecutionListener#taskStarted(de.renew.workflow.base.Task)
   */
  public void taskStarted( final Task task ) throws TaskExecutionException
  {
    if( m_activeTask != null )
    {
      if( !stopTask( m_activeTask ) )
        return;
    }
    final String name = task.getURI();
    final Command command = getCommand( m_commandService, name );
    final ContextType context = task.getContext();
    if( context != null )
    {
      activateContext( context );
    }
    try
    {
      m_handlerService.executeCommand( command.getId(), null );
    }
    catch( final NotHandledException e )
    {
      // ignore not handled
    }
    catch( final Throwable e )
    {
      throw new TaskExecutionException( e );
    }

    task.setState( EActivityExeState.RUNNING );
    m_activeTask = task;
    final String commandId = task.getURI();
    if( m_isWorkflowMode )
    {
      if( m_connector.canRequest( commandId ) )
      {
        m_connector.request( commandId );
        logger.info( "requested work item for " + commandId );
      }
      else
      {
        logger.info( "no work item available for " + commandId );
      }
    }
  }

  private ContextActivation activateContext( final ContextType context ) throws ContextActivationException
  {
    final ContextType parentContext = context.getParent();
    final IHandler handler = m_contextHandlerFactory.getHandler( context );
    final Command contextCommand = getCommand( m_commandService, context.getId() );
    contextCommand.setHandler( handler );
    ContextActivation parentActivation = null;
    if( parentContext != null )
    {
      parentActivation = activateContext( parentContext );
    }

    Object commandResult;
    try
    {
      commandResult = m_handlerService.executeCommand( context.getId(), null );
    }
    catch( final Throwable e )
    {
      throw new ContextActivationException( e );
    }

    if( parentActivation == null )
    {
      return new ContextActivation( context, commandResult );
    }
    else
    {
      return new ContextActivation( context, commandResult, parentActivation );
    }
  }

  private Command getCommand( final ICommandService commandService, final String commandId )
  {
    final Command command = commandService.getCommand( commandId );
    if( !command.isDefined() )
    {
      final Category category = commandService.getCategory( "org.kalypso.afgui.tasks" ); //$NON-NLS-1$
      if( !category.isDefined() )
      {
        category.define( TASKS_COMMANDS_CATEGORY, null );
      }
      command.define( commandId, null, category );
    }
    return command;
  }

  /**
   * @see de.renew.workflow.cases.ITaskExecutionListener#taskCompleted(de.renew.workflow.base.Task)
   */
  private boolean stopTask( final Task task )
  {
    final String commandId = task.getURI();
    if( m_authority.canStopTask( task ) )
    {
      final CaseData caseData = m_dataProvider.getCaseData();
      if( isRequiredDataAvailable( task, caseData ) && m_isWorkflowMode )
      {
        // confirm
        m_connector.confirm( commandId, caseData );
        logger.info( "confirmed activity for " + commandId );
      }
      else if( m_isWorkflowMode ) // && !isRequiredDataAvailable( task ))
      {
        // cancel
        m_connector.cancel( commandId );
        logger.info( "cancelled activity for " + commandId );
      }
      m_activeTask.setState( EActivityExeState.AVAILABLE );
      m_activeTask = null;
      return true;
    }
    else
    {
      return false;
    }
  }

  private boolean isRequiredDataAvailable( final Task task, final CaseData caseData )
  {
    // TODO check required case data
    return true;
  }
}