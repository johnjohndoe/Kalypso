package de.renew.workflow;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.IParameter;
import org.eclipse.core.commands.Parameterization;
import org.eclipse.core.commands.ParameterizedCommand;
import org.eclipse.core.commands.common.NotDefinedException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.IHandlerService;

/**
 * @author Stefan Kurzbach
 */
public class EclipseOperationDelegate implements ClientOperation
{
  private static final long serialVersionUID = 2794894971368488780L;

  static final Logger logger = Logger.getLogger( EclipseOperationDelegate.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "de.renew.workflow.connector/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  private final Map m_parameterMap;

  Proxy m_proxy;

  /**
   * @param runtimeParameter
   * @param perspectiveId
   * @param contextId
   * @param proxy
   */
  public EclipseOperationDelegate( final Map parameterMap, final Proxy proxy )
  {
    m_parameterMap = parameterMap;
    m_proxy = proxy;
  }

  /**
   * @see de.renew.workflow.ClientOperation#execute()
   */
  public Object execute( )
  {
    final ICommandService cmdService = (ICommandService) PlatformUI.getWorkbench().getService( ICommandService.class );
    final String cmdId = (String) m_parameterMap.get( CaseTask.PARAM_CMD_ID );
    final Command cmd = cmdService.getCommand( cmdId );
    logger.log( Level.INFO, "checking command " + cmdId );
    if( cmd.isDefined() && cmd.isEnabled() )
    {
      logger.log( Level.INFO, cmdId + " is defined" );
      final IHandlerService handlerService = (IHandlerService) PlatformUI.getWorkbench().getService( IHandlerService.class );
      IParameter[] parameters = null;
      try
      {
        parameters = cmd.getParameters();
      }
      catch( final NotDefinedException e1 )
      {
        // does not happen
      }
      if( parameters != null )
      {
        final List<Parameterization> parameterList = new ArrayList<Parameterization>();
        for( final IParameter param : parameters )
        {
          parameterList.add( new Parameterization( param, (String) m_parameterMap.get( param.getId() ) ) );
        }
        final ParameterizedCommand parameterizedCommand = new ParameterizedCommand( cmd, parameterList.toArray( new Parameterization[parameterList.size()] ) );
        Display.getDefault().asyncExec( new Runnable()
        {
          public void run( )
          {
            try
            {

              final Object result = handlerService.executeCommand( parameterizedCommand, null );
              m_proxy.notifyCompletion( result );
            }
            catch( Exception e )
            {
              logger.log( Level.SEVERE, "command could not be executed", e );
            }
          }
        } );
      }
      else
      {
        Display.getDefault().asyncExec( new Runnable()
        {
          public void run( )
          {
            try
            {
              final Object result = handlerService.executeCommand( cmdId, null );
              m_proxy.notifyCompletion( result );
            }
            catch( Exception e )
            {
              logger.log( Level.SEVERE, "command could not be executed", e );
            }
          }
        } );
      }
    }

    return null;
  }
}
