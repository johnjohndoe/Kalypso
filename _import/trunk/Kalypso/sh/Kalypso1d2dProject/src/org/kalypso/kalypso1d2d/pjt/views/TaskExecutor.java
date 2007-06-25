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
package org.kalypso.kalypso1d2d.pjt.views;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.xml.bind.JAXBElement;

import org.eclipse.core.commands.Category;
import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.commands.NotHandledException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypso1d2d.pjt.perspective.Perspective;
import org.kalypso.kalypso1d2d.pjt.perspective.PerspectiveWatcher;
import org.kalypso.ogc.gml.outline.GisMapOutlineView;

import de.renew.workflow.base.Task;
import de.renew.workflow.base.TaskGroup;
import de.renew.workflow.connector.cases.TaskExecutionException;
import de.renew.workflow.connector.context.ContextActivation;
import de.renew.workflow.connector.context.ContextActivationException;
import de.renew.workflow.connector.worklist.ITaskExecutionAuthority;
import de.renew.workflow.connector.worklist.ITaskExecutor;
import de.renew.workflow.connector.worklist.TaskExecutionListener;
import de.renew.workflow.contexts.WorkbenchSiteContext;
import de.renew.workflow.contexts.ContextType;
import de.renew.workflow.contexts.IContextHandlerFactory;
import de.renew.workflow.contexts.WorkbenchPartContextType;
import de.renew.workflow.contexts.ViewContext;

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

  public TaskExecutor( final IContextHandlerFactory contextHandlerFactory, final ITaskExecutionAuthority authority, final ICommandService commandService, final IHandlerService handlerService )
  {
    m_contextHandlerFactory = contextHandlerFactory;
    m_authority = authority;
    m_commandService = commandService;
    m_handlerService = handlerService;
  }

  public Task getActiveTask( )
  {
    return m_activeTask;
  }

  /**
   * @see de.renew.workflow.connector.ITaskExecutor#execute(de.renew.workflow.base.Task)
   */
  public void execute( final Task task ) throws TaskExecutionException
  {
    // HACK: in order to make sure the SzenarioSourceProvider is initialized, call this method each time
    // a task is executed. It would be better if this happened automatically on plug-in-start, but
    // TODO: is this still necessary?
    Kalypso1d2dProjectPlugin.getDefault().getDataProvider();

    if( m_activeTask != null )
    {
      if( !m_authority.canStopTask( m_activeTask ) )
        return;
    }
    final String name = task.getURI();
    final Command command = getCommand( m_commandService, name, task instanceof TaskGroup ? TaskExecutionListener.CATEGORY_TASKGROUP : TaskExecutionListener.CATEGORY_TASK );
    final ContextType context = task.getContext();
    if( context != null )
    {
      activateContext( context );
      final Collection<String> partsToKeep = collectOpenedViews( context );
      partsToKeep.add( WorkflowView.ID );
      partsToKeep.add( Perspective.SCENARIO_VIEW_ID );
      partsToKeep.add( GisMapOutlineView.ID );
      PerspectiveWatcher.cleanPerspective( PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage(), partsToKeep );
    }
    try
    {
      m_handlerService.executeCommand( command.getId(), null );
    }
    catch( final NotHandledException e )
    {
      // e.printStackTrace();
      // if not handled, last context activation gives result
      // if( activateContext != null )
      // {
      // result = activateContext.getResult();
      // }
    }
    catch( final Throwable e )
    {
      throw new TaskExecutionException( e );
    }
    m_activeTask = task;
  }

  private Collection<String> collectOpenedViews( final ContextType context )
  {
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
    if( context instanceof ViewContext )
      result.add( ((ViewContext) context).getPartId() );
    else if( context instanceof WorkbenchSiteContext )
    {
      final WorkbenchSiteContext multiContext = (WorkbenchSiteContext) context;
      final List<JAXBElement< ? extends WorkbenchPartContextType>> subContexts = multiContext.getPartContexts();
      for( final JAXBElement< ? extends WorkbenchPartContextType> element : subContexts )
      {
        final ContextType value = element.getValue();
        if( value instanceof ViewContext )
          result.add( ((ViewContext) value).getPartId() );
      }
    }
    return result;
  }

  private ContextActivation activateContext( final ContextType context ) throws ContextActivationException
  {
    final IHandler handler = m_contextHandlerFactory.getHandler( context );
    final ContextType parentContext = context.getParent();
    ContextActivation parentActivation = null;
    if( parentContext != null )
    {
      parentActivation = activateContext( parentContext );
    }

    final Command contextCommand = getCommand( m_commandService, context.getId(), TaskExecutionListener.CATEGORY_CONTEXT );
    contextCommand.setHandler( handler );
    Object commandResult;
    try
    {
      commandResult = m_handlerService.executeCommand( context.getId(), null );
    }
    catch( final Throwable e )
    {
      throw new ContextActivationException( e );
    }

    if( parentContext == null )
    {
      return new ContextActivation( context, commandResult );
    }
    else
    {
      return new ContextActivation( context, commandResult, parentActivation );
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
}
