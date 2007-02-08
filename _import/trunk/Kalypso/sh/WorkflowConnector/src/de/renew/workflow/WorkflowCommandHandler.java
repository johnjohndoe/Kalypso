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
package de.renew.workflow;

import java.util.logging.Logger;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;

/**
 * @author Stefan Kurzbach
 */
public abstract class WorkflowCommandHandler extends AbstractHandler
{
  private WorkflowConnector m_connector;

  protected static Logger logger = Logger.getLogger( WorkflowCommandHandler.class.getName() );

  private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "de.renew.workflow.connector/debug" ) );

  static
  {
    if( !log )
      logger.setUseParentHandlers( false );
  }

  /**
   * Implement your code in executeInternal.
   * 
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public final Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final String commandId = event.getCommand().getId();
    if( WorkflowConnector.isWorkflowMode() )
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
    final IStatus status;
    try
    {
      status = executeInternal( event );
    }
    catch( final CoreException e )
    {
      throw new ExecutionException( "Problem in internal execution: " + e.getLocalizedMessage(), e );
    }
    catch( final Throwable t )
    {
      throw new ExecutionException( "Problem in internal execution: " + t.getLocalizedMessage(), t );
    }

    if( status.isOK() )
    {
      if( WorkflowConnector.isWorkflowMode() )
      {
        m_connector.confirm( commandId, status.getMessage() );
        logger.info( "confirmed activity for " + commandId );
      }
    }
    else
    {
      if( WorkflowConnector.isWorkflowMode() )
      {
        m_connector.cancel( commandId );
        logger.info( "cancelled activity for " + commandId );
      }
    }

    return status;
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#isEnabled()
   */
  @Override
  public final boolean isEnabled( )
  {
    return WorkflowConnector.checkWorkflowMode();
  }

  /**
   * Implement this method instead of execute(*) for your command code
   */
  protected abstract IStatus executeInternal( final ExecutionEvent event ) throws CoreException;
}