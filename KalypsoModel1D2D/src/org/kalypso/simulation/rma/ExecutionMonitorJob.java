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
package org.kalypso.simulation.rma;

import java.io.IOException;
import java.io.InputStream;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;

import net.opengeospatial.ows.ExceptionReport;
import net.opengeospatial.wps.ExecuteResponseType;
import net.opengeospatial.wps.ProcessFailedType;
import net.opengeospatial.wps.ProcessStartedType;
import net.opengeospatial.wps.StatusType;

import org.apache.commons.io.IOUtils;
import org.apache.commons.vfs.FileContent;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.FileSystemManagerWrapper;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.progress.IProgressConstants;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.service.wps.utils.MarshallUtilities;
import org.kalypso.service.wps.utils.WPSUtilities;

/**
 * Monitors an execute response file until canceled, finished or error
 * 
 * @author kurzbach
 */
public class ExecutionMonitorJob extends Job
{
  private FileSystemManagerWrapper m_fileSystemManager = null;

  private IStatus m_status = null;

  private ExecuteResponseType m_executeResponse = null;

  private int m_previousWork;

  private String m_statusLocation;

  public ExecutionMonitorJob( final String statusLocation )
  {
    this( statusLocation, null );
  }

  public ExecutionMonitorJob( final String statusLocation, final String commandId )
  {
    super( statusLocation );
    updateStatusLocation( statusLocation );

    setProperty( IProgressConstants.NO_IMMEDIATE_ERROR_PROMPT_PROPERTY, Boolean.TRUE );
    setProperty( IProgressConstants.KEEP_PROPERTY, Boolean.TRUE );
    setUser( true );

// FIXME: Compile error in Eclipse 3.5.1
// if( commandId != null )
// {
// final ICommandService commandSvc = (ICommandService) PlatformUI.getWorkbench().getAdapter( ICommandService.class );
// final Command command = commandSvc.getCommand( commandId );
// if( command.isDefined() )
// {
// try
// {
//          final IParameter parmDef = command.getParameter( "statusLocation" ); //$NON-NLS-1$
// final Parameterization[] parms = new Parameterization[] { new Parameterization( parmDef, statusLocation ) };
// final ParameterizedCommand pCommand = new ParameterizedCommand( command, parms );
// setProperty( IProgressConstants.COMMAND_PROPERTY, pCommand );
// }
// catch( final NotDefinedException e )
// {
// // should never happen
// }
// }
// }

    // display the status if clicked
// setProperty( IProgressConstants.ACTION_PROPERTY, new Action( "Show current status" )
// {
// @Override
// public void run( )
// {
// final IStatus status = getStatus();
// if( status == null )
// return;
//
// final IWorkbench workbench = PlatformUI.getWorkbench();
// if( workbench == null )
// return;
//
// final IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
// if( activeWorkbenchWindow == null )
// return;
//
// // open new status dialog
// final Shell shell = activeWorkbenchWindow.getShell();
// final StatusDialog dialog = new StatusDialog( shell, status, "Current status" );
// dialog.open();
// }
// } );
  }

  /**
   * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  @SuppressWarnings("unchecked")
  protected IStatus run( IProgressMonitor monitor )
  {
    // check for null progress monitor
    if( monitor == null )
      monitor = new NullProgressMonitor();

    monitor.beginTask( "Monitoring...", 100 );

    try
    {
      canceledLoop: while( !monitor.isCanceled() )
      {
        InputStream inputStream = null;
        FileObject statusFile = null;
        try
        {
          // lazily initialize file system manager, do not forget to dispose when the job is not needed anymore
          if( m_fileSystemManager == null )
            m_fileSystemManager = VFSUtilities.getNewManager();

          // resolve file with proxy
          statusFile = VFSUtilities.checkProxyFor( m_statusLocation, m_fileSystemManager );
          if( statusFile.exists() )
          {
            final FileContent content = statusFile.getContent();
            inputStream = content.getInputStream();
            final String xml = IOUtils.toString( inputStream );
            if( xml != null && !xml.isEmpty() )
            {
              final Object object = MarshallUtilities.unmarshall( xml );
              final JAXBElement<ExecuteResponseType> executeState = (JAXBElement<ExecuteResponseType>) object;
              m_executeResponse = executeState.getValue();
            }
          }
        }
        catch( final IOException e )
        {
          return StatusUtilities.statusFromThrowable( e );
        }
        catch( final JAXBException e )
        {
          return StatusUtilities.statusFromThrowable( e );
        }
        finally
        {
          IOUtils.closeQuietly( inputStream );
          try
          {
            statusFile.close();
          }
          catch( final FileSystemException e )
          {
            // gobble
          }
        }

        if( m_executeResponse != null )
        {
          // evaluate execute response
          final StatusType state = m_executeResponse.getStatus();
          final String processAccepted = state.getProcessAccepted();
          final ProcessFailedType processFailed = state.getProcessFailed();
          final ProcessStartedType processStarted = state.getProcessStarted();
          final String processSucceeded = state.getProcessSucceeded();

          // create new status
          final IStatus status;
          if( processAccepted != null )
          {
            status = StatusUtilities.createInfoStatus( processAccepted );
            monitor.subTask( processAccepted );
          }
          else if( processFailed != null )
          {
            final ExceptionReport exceptionReport = processFailed.getExceptionReport();
            final String messages = WPSUtilities.createErrorString( exceptionReport );
            status = StatusUtilities.createErrorStatus( messages );
            monitor.done();
            break canceledLoop;
          }
          else if( processStarted != null )
          {
            final String description = processStarted.getValue();
            final Integer percent = processStarted.getPercentCompleted();
            final int percentCompleted = percent != null ? percent.intValue() : 0;
            monitor.worked( percentCompleted - m_previousWork );
            m_previousWork = percentCompleted;
            status = StatusUtilities.createInfoStatus( "%s (%d%% completed)", description, percentCompleted );
            monitor.subTask( description );
          }
          else if( processSucceeded != null )
          {
            status = StatusUtilities.createInfoStatus( processSucceeded );
            monitor.done();
            break canceledLoop;
          }
          else
          {
            status = StatusUtilities.createWarningStatus( "Unknown state" );
          }

          // update status
          updateStatusLocation( m_executeResponse.getStatusLocation() );
          setStatus( status );
          monitor.subTask( status.getMessage() );
        }

        Thread.sleep( 500 );
      }
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
    }
    finally
    {
      // clean up
      m_fileSystemManager.close();
    }

    return m_status;
  }

  private void setStatus( final IStatus status )
  {
    m_status = status;
  }

  public IStatus getStatus( )
  {
    return m_status;
  }

  private void updateStatusLocation( final String statusLocation )
  {
    m_statusLocation = statusLocation;
    setName( "Monitoring execute response at " + statusLocation );
  }

  public ExecuteResponseType getExecuteResponse( )
  {
    return m_executeResponse;
  }

}
