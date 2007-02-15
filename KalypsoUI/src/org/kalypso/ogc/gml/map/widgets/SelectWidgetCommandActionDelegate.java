/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/

/*******************************************************************************
 * Copyright (c) 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.kalypso.ogc.gml.map.widgets;

import java.util.Map;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.CommandEvent;
import org.eclipse.core.commands.ICommandListener;
import org.eclipse.core.commands.Parameterization;
import org.eclipse.core.commands.ParameterizedCommand;
import org.eclipse.core.commands.common.NotDefinedException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IActionDelegate2;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * This action delegate can be used to specify a widget and a context to call and update a {@link Command}. Handlers
 * that wish to handle the command must register for the command id
 * "org.kalypso.ogc.gml.map.widgets.SelectWidgetCommand#context" and subclass {@link SelectWidgetHandler}. The
 * specified widget will then be called from the command. The context can be used to set the active state of the
 * handler. <br>
 * <br>
 * Example:<br>
 * <blockquote> <br>
 * <action id="xp.CreateGridDelegate"> <br>
 * <class class="org.kalypso.ogc.gml.map.widgets.SelectWidgetCommandActionDelegate"> <br>
 * <parameter name="org.kalypso.ogc.gml.map.widgets.SelectWidgetCommand.widget" value="xp.CreateGitterWidget"/>
 * <parameter name="org.kalypso.ogc.gml.map.widgets.SelectWidgetCommand.context" value="MyContext"/> <br>
 * <parameter name="org.kalypso.ogc.gml.map.widgets.SelectWidgetCommand.plugin" value="myPlugin"/> <br>
 * </class> </action> <br>
 * <handler class="MySelectWidgetHandler" <br>
 * commandId="org.kalypso.ogc.gml.map.widgets.SelectWidgetCommand#MyContext"> <br>
 * <activeWhen> <with variable="activeContexts"> <iterate operator="or"> <equals value="MyContext"/> </iterate> </with>
 * </activeWhen> </handler> </blockcode>
 */
public class SelectWidgetCommandActionDelegate implements IWorkbenchWindowActionDelegate, IViewActionDelegate, IEditorActionDelegate, IObjectActionDelegate, IExecutableExtension, ICommandListener,
    IActionDelegate2
{
  private static final String COMMAND_ID = "org.kalypso.ogc.gml.map.widgets.SelectWidgetCommand";

  private static final String PARAM_CONTEXT = COMMAND_ID + ".context";

  public static final String PARAM_WIDGET_CLASS = COMMAND_ID + ".widget";

  public static final String PARAM_PLUGIN_ID = COMMAND_ID + ".plugin";

  private String context = null;

  private String widgetClass = null;

  private String pluginId;

  private ParameterizedCommand m_parameterizedCommand = null;

  private IHandlerService m_handlerService = null;

  private IAction m_action;

  private IActionBars m_actionBars;

  /**
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
   */
  public void dispose( )
  {
    m_handlerService = null;
    m_parameterizedCommand = null;
    m_action = null;
    m_actionBars = null;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( IAction action )
  {
    if( m_handlerService == null )
    {
      return;
    }
    if( m_parameterizedCommand != null )
    {
      try
      {
        m_handlerService.executeCommand( m_parameterizedCommand, null );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
    // we don't care, handlers get their selection from the
    // ExecutionEvent application context
  }

  /**
   * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  public void setInitializationData( IConfigurationElement config, String propertyName, Object data ) throws CoreException
  {
    if( data instanceof Map )
    {
      Map parameterMap = (Map) data;
      widgetClass = (String) parameterMap.get( PARAM_WIDGET_CLASS );
      context = (String) parameterMap.get( PARAM_CONTEXT );
      pluginId = (String) parameterMap.get( PARAM_PLUGIN_ID );
    }

    if( widgetClass == null || context == null || pluginId == null )
    {
      String id = config.getAttribute( "id" );
      Status status = new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), SWT.OK, "The '" + id + "' action won't work without a context, a widget class and a plugin id.", null );
      throw new CoreException( status );
    }
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#init(org.eclipse.ui.IWorkbenchWindow)
   */
  public void init( final IWorkbenchWindow window )
  {
    if( m_handlerService != null )
    {
      // already initialized
      return;
    }

    final IWorkbench workbench = window.getWorkbench();
    m_handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final ICommandService commandService = (ICommandService) workbench.getService( ICommandService.class );
    final Command command = createCommand( commandService );
    updateActionState( command );
  }

  /**
   * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
   */
  public void init( final IViewPart view )
  {
    m_actionBars = view.getViewSite().getActionBars();
    init( view.getSite().getWorkbenchWindow() );
  }

  /**
   * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IEditorPart)
   */
  public void setActiveEditor( final IAction action, final IEditorPart targetEditor )
  {
    if( targetEditor != null )
    {
      m_actionBars = targetEditor.getEditorSite().getActionBars();
      init( targetEditor.getSite().getWorkbenchWindow() );
    }
  }

  /**
   * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IWorkbenchPart)
   */
  public void setActivePart( final IAction action, final IWorkbenchPart targetPart )
  {
    if( targetPart != null )
    {
      init( targetPart.getSite().getWorkbenchWindow() );
    }
  }

  /**
   * @see org.eclipse.core.commands.ICommandListener#commandChanged(org.eclipse.core.commands.CommandEvent)
   */
  public void commandChanged( final CommandEvent commandEvent )
  {
    if( m_action != null )
    {
      updateActionState( commandEvent.getCommand() );
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate2#init(org.eclipse.jface.action.IAction)
   */
  public void init( final IAction action )
  {
    m_action = action;
    updateActionState( null );
  }

  /**
   * @see org.eclipse.ui.IActionDelegate2#runWithEvent(org.eclipse.jface.action.IAction, org.eclipse.swt.widgets.Event)
   */
  public void runWithEvent( IAction action, Event event )
  {
    run( action );
  }

  private Command createCommand( final ICommandService commandService )
  {
    final String cmdName = COMMAND_ID + "#" + context;
    final Command cmd = commandService.getCommand( cmdName );
    try
    {
      if( !cmd.isDefined() )
      {
        final Command selectWidgetCmd = commandService.getCommand( COMMAND_ID );
        cmd.define( cmdName, cmdName, selectWidgetCmd.getCategory(), selectWidgetCmd.getParameters() );
      }      
      cmd.addCommandListener( this );
      m_parameterizedCommand = new ParameterizedCommand( cmd, new Parameterization[] { new Parameterization( cmd.getParameter( PARAM_WIDGET_CLASS ), widgetClass ),
          new Parameterization( cmd.getParameter( PARAM_PLUGIN_ID ), pluginId ) } );
    }
    catch( final NotDefinedException e )
    {
      e.printStackTrace();
    }
    return cmd;
  }

  private void updateActionState( final Command command )
  {
    final boolean enabledState = command != null ? command.isEnabled() : false;

    m_action.setEnabled( enabledState );

    // final String actionId = m_action.getId();
    // final IContributionManager toolBarManager = m_actionBars.getToolBarManager();
    // final IContributionManager menuManager = m_actionBars.getMenuManager();
    // final IContributionItem toolbarContribution = toolBarManager.find( actionId );
    // if( toolbarContribution != null )
    // {
    // toolbarContribution.setVisible( enabledState );
    // toolBarManager.update( true );
    // }
    // final IContributionItem menuContribution = menuManager.find( actionId );
    // if( menuContribution != null )
    // {
    // menuContribution.setVisible( enabledState );
    // menuManager.update( true );
    // }
    // m_actionBars.updateActionBars();
  }
}
