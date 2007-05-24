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
package org.kalypso.ui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.CommandEvent;
import org.eclipse.core.commands.ICommandListener;
import org.eclipse.core.commands.IParameter;
import org.eclipse.core.commands.Parameterization;
import org.eclipse.core.commands.ParameterizedCommand;
import org.eclipse.core.commands.common.NotDefinedException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IContributionManager;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelection;
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
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;

/**
 * TODO this class is duplicate with de.renew.workflow.connector.context.handlers.GenericCommandActionDelegate
 * Move it into one of the contribution plug-ins
 * 
 * @author Stefan Kurzbach
 */
public class GenericCommandActionDelegate implements IWorkbenchWindowActionDelegate, IViewActionDelegate, IEditorActionDelegate, IObjectActionDelegate, IExecutableExtension, ICommandListener,
    IActionDelegate2
{
  private static final class UpdateActionbarsJob extends UIJob
  {
    private final IActionBars m_bars;

    private final String m_id;

    private final boolean m_state;

    public UpdateActionbarsJob( final String name, final IActionBars bars, final String id, final boolean state )
    {
      super( name );
      m_bars = bars;
      m_id = id;
      m_state = state;
    }

    @Override
    public IStatus runInUIThread( final IProgressMonitor monitor )
    {
      // TODO holger schreib mal was...
           
      final IContributionManager toolBarManager = m_bars.getToolBarManager();
      final IContributionManager menuManager = m_bars.getMenuManager();

      final IContributionItem toolbarContribution = toolBarManager.find( m_id );
      if( toolbarContribution != null )
      {
        toolbarContribution.setVisible( m_state );
        toolBarManager.update( true );
      }
      final IContributionItem menuContribution = menuManager.find( m_id );
      if( menuContribution != null )
      {
        menuContribution.setVisible( m_state );
        menuManager.update( true );
      }

      m_bars.updateActionBars();
      
      
      return Status.OK_STATUS;
    }
  }

  private static final Object PARAM_COMMAND_ID = "commandId";

  ParameterizedCommand m_parameterizedCommand = null;

  private IHandlerService m_handlerService = null;

  private IAction m_action;

  private IActionBars m_actionBars;

  private String m_commandId;

  private final Map<String, String> m_parameterMap = new HashMap<String, String>();

  /**
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
   */
  public void dispose( )
  {
    if( m_parameterizedCommand != null )
      m_parameterizedCommand.getCommand().removeCommandListener( this );

    m_handlerService = null;
    m_parameterizedCommand = null;
    m_action = null;
    m_actionBars = null;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    // we don't care, handlers get their selection from the
    // ExecutionEvent application context
    m_action = action;
    updateActionState();
  }

  /**
   * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  @SuppressWarnings( { "unchecked", "unused" })
  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data ) throws CoreException
  {
    if( data instanceof String )
    {
      m_commandId = (String) data;
    }
    else if( data instanceof Map )
    {
      m_parameterMap.putAll( (Map<String, String>) data );
      m_commandId = m_parameterMap.get( PARAM_COMMAND_ID );
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
    m_parameterizedCommand = createCommand( commandService );

    if( m_parameterizedCommand != null )
      m_parameterizedCommand.getCommand().addCommandListener( this );

    updateActionState();
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

    m_action = action;
    updateActionState();
  }

  /**
   * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IWorkbenchPart)
   */
  public void setActivePart( final IAction action, final IWorkbenchPart targetPart )
  {
    if( targetPart != null )
      init( targetPart.getSite().getWorkbenchWindow() );

    m_action = action;
    updateActionState();
  }

  /**
   * @see org.eclipse.core.commands.ICommandListener#commandChanged(org.eclipse.core.commands.CommandEvent)
   */
  public void commandChanged( final CommandEvent commandEvent )
  {
    updateActionState();
  }

  /**
   * @see org.eclipse.ui.IActionDelegate2#init(org.eclipse.jface.action.IAction)
   */
  public void init( final IAction action )
  {
    m_action = action;
    updateActionState();
  }

  /**
   * @see org.eclipse.ui.IActionDelegate2#runWithEvent(org.eclipse.jface.action.IAction, org.eclipse.swt.widgets.Event)
   */
  public void runWithEvent( final IAction action, final Event event )
  {
    /* Do not run for unchecked radio-buttons */
    if( (action.getStyle() & IAction.AS_RADIO_BUTTON) != 0 && !action.isChecked() )
      return;

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
      catch( final Throwable e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoGisPlugin.getDefault().getLog().log( status );
        ErrorDialog.openError( event.display.getActiveShell(), action.getText(), "Operation konnte nicht ausgeführt werden.", status );
      }
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    throw new UnsupportedOperationException();
  }

  protected ParameterizedCommand createCommand( final ICommandService commandService )
  {
    final Command command = commandService.getCommand( m_commandId );
    try
    {
      if( !command.isDefined() )
        command.define( m_commandId, m_commandId, commandService.getCategory( "org.kalypso.ui.commands.default" ) );

      final List<Parameterization> parameters = new ArrayList<Parameterization>();
      for( final String parmName : m_parameterMap.keySet() )
      {
        if( PARAM_COMMAND_ID.equals( parmName ) )
          continue;

        final IParameter parm = command.getParameter( parmName );
        if( parm == null )
        {
          // asking for a bogus parameter? No problem
          continue;
        }

        parameters.add( new Parameterization( parm, m_parameterMap.get( parmName ) ) );
      }
      return new ParameterizedCommand( command, parameters.toArray( new Parameterization[parameters.size()] ) );
    }
    catch( final NotDefinedException e )
    {
      e.printStackTrace();
    }
    return null;
  }

  private void updateActionState( )
  {
    if( m_action == null || m_actionBars == null )
      return;

    final Command command = m_parameterizedCommand == null ? null : m_parameterizedCommand.getCommand();

    final boolean enabledState = command != null ? command.isEnabled() : false;

    m_action.setEnabled( enabledState );

    final String actionId = m_action.getId();
    final IActionBars actionBars = m_actionBars;

    final UIJob job = new UpdateActionbarsJob( "Update Action-Bars", actionBars, actionId, enabledState );
    job.setPriority( UIJob.INTERACTIVE );
//    job.schedule();
    job.schedule(200);
    //TODO: just delay, when the map view gets the focus by pressing an allready existing button.
  }
}
