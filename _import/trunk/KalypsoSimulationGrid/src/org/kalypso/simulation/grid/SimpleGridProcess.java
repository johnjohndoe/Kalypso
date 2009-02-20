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
package org.kalypso.simulation.grid;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemManager;
import org.apache.commons.vfs.Selectors;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.process.IProcess;
import org.kalypso.commons.process.ProcessTimeoutException;
import org.kalypso.contribs.java.lang.ICancelable;

import de.unihannover.rvs.gdi.jobsubmit.impl.GDIFileTransfer;
import de.unihannover.rvs.gdi.jobsubmit.impl.GDIJobFactory;
import de.unihannover.rvs.gdi.jobsubmit.impl.GDIJobProperties;
import de.unihannover.rvs.gdi.jobsubmit.impl.GDIState;
import de.unihannover.rvs.gdi.jobsubmit.interfaces.GDIJob;
import de.unihannover.rvs.gdi.jobsubmit.interfaces.GDIObserver;
import de.unihannover.rvs.gdi.jobsubmit.interfaces.GDIObserverSubject;

/**
 * This implementation executes processes in a computing grid.
 * 
 * @author Stefan Kurzbach
 */
public class SimpleGridProcess implements IProcess, GDIObserver
{
  private final FileObject m_workingDir;

  private final URL m_exeUrl;

  private final List<String> m_commandLine = new ArrayList<String>();

  private GDIJob m_job = GDIJobFactory.createGDIJob( GDIJobFactory.defaultGDIJob );

  private final Map<String, String> m_environment = new HashMap<String, String>();

  private IProgressMonitor m_monitor;

  private GDIState m_status = GDIState.UNSUBMITTED;

  private long m_timeout;

  /**
   * Creates a new grid process that gets its files from workingDir and runs the executable exeUrl on a computing grid
   * server as an asynchronous job
   * 
   * @param workingDir
   * @param exeUrl
   * @param commandlineArgs
   */
  public SimpleGridProcess( final FileObject workingDir, final URL exeUrl, final String... commandlineArgs )
  {
    Assert.isNotNull( workingDir );
    Assert.isNotNull( exeUrl );

    m_workingDir = workingDir;
    m_exeUrl = exeUrl;

    if( commandlineArgs != null )
    {
      for( final String arg : commandlineArgs )
        m_commandLine.add( arg );
    }
  }

  /**
   * Sets a progress monitor.
   * 
   * @see org.kalypso.commons.process.IProcess#setProgressMonitor(org.eclipse.core .runtime.IProgressMonitor)
   */
  @Override
  public void setProgressMonitor( final IProgressMonitor monitor )
  {
    m_monitor = monitor;
  }

  /**
   * Gets the grid job's environment
   * 
   * @see org.kalypso.commons.process.IProcess#environment()
   */
  @Override
  public Map<String, String> environment( )
  {
    return m_environment;
  }

  /**
   * @see org.kalypso.commons.process.IProcess#setTimeout(long)
   */
  @Override
  public void setTimeout( final long timeout )
  {
    m_timeout = timeout;
  }

  /**
   * @see org.kalypso.commons.process.IProcess#startProcess(java.io.OutputStream, java.io.OutputStream,
   *      java.io.InputStream, org.kalypso.contribs.java.lang.ICancelable)
   */
  @Override
  public int startProcess( final OutputStream stdOut, final OutputStream stdErr, final InputStream stdIn, final ICancelable cancelable ) throws ProcessTimeoutException, IOException
  {
    final FileSystemManager manager = VFSUtilities.getManager();

    // stage-in the exe to remote place
    // try to convert bundle resouce url to local file url
    final String exeUrlName = FileLocator.toFileURL( m_exeUrl ).toString();
    final FileObject exeFile = manager.resolveFile( exeUrlName );
    final File localExeFile = exeFile.getFileSystem().replicateFile( exeFile, Selectors.SELECT_SELF );
    final String exeBaseName = FileUtilities.nameFromPath( exeUrlName );
    final List<GDIFileTransfer> stageInFiles = new ArrayList<GDIFileTransfer>();
    stageInFiles.add( new GDIFileTransfer( localExeFile.toURI().toString(), m_workingDir.resolveFile( exeBaseName ).getName().getURI() ));
    
    // initialize simulation monitor if not set
    // TODO check monitor
    m_monitor = SubMonitor.convert( m_monitor, String.format( "Running grid process for executable %s in directoy %s.", exeUrlName, m_workingDir.getName().getFriendlyURI() ), 10 );

    
    /* configure and submit grid job */

    // logging output
    final String stdOutFileName = exeBaseName + ".out";
    final String stdErrFileName = exeBaseName + ".err";
    final Path rootWorkingPath = new Path(m_workingDir.getName().getRootURI());
    final String targetHostName = rootWorkingPath.lastSegment();
// final String targetHostName = GridProcessFactory.GRID_SERVER_URL;

    final String queue = "dgitest"; // for hannover
    final String factory = "PBS"; // for hannover

    // final String factory = "SGE" // for kaiserslautern

    final String sandBoxDir = m_workingDir.getName().getBaseName();

    final GDIJobProperties props = new GDIJobProperties();
    props.addPreference( "gdi.targethostname", targetHostName );
    props.addPreference( "gdi.executable", "./" + exeBaseName );
    props.addPreference( "gdi.arguments", m_commandLine );
    props.addPreference( "gdi.stdout", stdOutFileName );
    props.addPreference( "gdi.stderr", stdErrFileName );
    props.addPreference( "gdi.filestagein", stageInFiles );
    props.addPreference( "gdi.factory", factory );
    props.addPreference( "gdi.queue", queue );
    props.addPreference( "gdi.sandboxdir", sandBoxDir );
    props.addPreference( "gdi.environment", m_environment );
    m_job.setProps( props );

    m_monitor.subTask( String.format( "Submitting grid job to server %s.", targetHostName ) );

    final PrintWriter stdPrinter = new PrintWriter( stdOut );
    final PrintWriter errPrinter = new PrintWriter( stdErr );

    // register observer
    ((GDIObserverSubject) m_job).registerObserver( this );
    int returnCode = Status.ERROR;
    try
    {
      m_job.submit();

      final long startTime = System.currentTimeMillis();
      finished: while( !(m_monitor.isCanceled()) )
      {
        if( m_timeout > 0 )
        {
          final long currentTime = System.currentTimeMillis();
          final long processDuration = currentTime - startTime;
          if( processDuration > m_timeout )
          {
            // job will be canceled in finish statement of try/catch block
            throw new ProcessTimeoutException();
          }
        }
        switch( m_status )
        {
          case RUNNING:
          case UNSUBMITTED:
          case STAGEIN:
          case STAGEOUT:
            sleep();
            break;
          case FINISHED:
            returnCode = IStatus.OK;
          case FAILED:
            break finished;
        }
      }
    }
    catch( final ProcessTimeoutException e )
    {
      // rethrow if process timed out
      throw e;
    }
    catch( final Throwable e )
    {
      e.printStackTrace( errPrinter );
    }
    finally
    {
      // clean up job in case of an error
      // this will have no effect if job has finished successfully
      if( m_job != null && m_job.getStatus() == GDIState.RUNNING )
        m_job.cancelJob();
      m_job = null;

      if( m_monitor.isCanceled() || (cancelable != null && cancelable.isCanceled()) )
      {
        errPrinter.println( "Job has been canceled." );
        throw new OperationCanceledException();
      }
    }

    IOUtils.closeQuietly( stdPrinter );
    IOUtils.closeQuietly( errPrinter );

    return returnCode;
  }

  private void sleep( )
  {
    try
    {
      Thread.sleep( 1000 );
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
    }
  }

  /*
   * (non-Javadoc)
   * @see de.unihannover.rvs.gdi.jobsubmit.interfaces.GDIObserver#update(org.gridlab .gat.resources.Job.JobState)
   */
  public void update( final GDIState newStatus )
  {
    m_status = newStatus;
    m_monitor.subTask( m_status.toString() );
  }

}
