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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.security.auth.Subject;

import org.apache.commons.io.IOUtils;
import org.apache.commons.vfs.FileName;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystem;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.FileSystemManagerWrapper;
import org.apache.commons.vfs.provider.gsiftp.GsiFtpFileObject;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.gridforum.jgss.ExtendedGSSManager;
import org.ietf.jgss.GSSCredential;
import org.ietf.jgss.GSSException;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.process.IProcess;
import org.kalypso.commons.process.ProcessTimeoutException;
import org.kalypso.contribs.java.lang.ICancelable;

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
  private static final String GRID_SERVER_HANNOVER = "gramd1.gridlab.uni-hannover.de";

  @SuppressWarnings("unused")
  private static final String GRID_SERVER_KAISERSLAUTERN = "gt4-gdi.sugi.uni-kl.de";

  private final String m_targetHostName;

  private final FileObject m_workingDir;

  private final String m_executable;

  private long m_timeout;

  private final List<String> m_commandLine = new ArrayList<String>();

  private final Map<String, String> m_environment = new HashMap<String, String>();

  private GDIState m_status = GDIState.UNSUBMITTED;

  private final FileSystemManagerWrapper m_manager;

// private IProgressMonitor m_monitor;

  /**
   * Creates a new grid process with given sandbox, executable and arguments.
   * 
   * @param tempDirName
   * @param exeUrl
   * @param commandlineArgs
   */
  public SimpleGridProcess( final String tempDirName, final String executable, final String... commandLineArgs ) throws IOException
  {
    Assert.isNotNull( tempDirName );
    Assert.isNotNull( executable );

    m_executable = executable;
    m_targetHostName = GRID_SERVER_HANNOVER;
    // m_targetHostName = GRID_SERVER_KAISERSLAUTERN;

    if( commandLineArgs != null )
    {
      for( final String arg : commandLineArgs )
      {
        m_commandLine.add( arg );
      }
    }

    m_manager = VFSUtilities.getNewManager();
    m_workingDir = createSandbox( tempDirName );
  }

  /**
   * @see org.kalypso.commons.process.IProcess#getSandboxDirectory()
   */
  @Override
  public String getSandboxDirectory( )
  {
    final FileName name = m_workingDir.getName();
    return name.getURI();
  }

// /**
// * Sets a progress monitor.
// *
// * @see org.kalypso.commons.process.IProcess#setProgressMonitor(org.eclipse.core .runtime.IProgressMonitor)
// */
// @Override
// public void setProgressMonitor( final IProgressMonitor monitor )
// {
// m_monitor = monitor;
// }

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
    // initialize simulation monitor if not set
    // final String friendlyURI = m_workingDir.getName().getFriendlyURI();
    // m_monitor = SubMonitor.convert( m_monitor, String.format(
    // "Running grid process for executable %s in directoy %s.", m_executable, friendlyURI ), 10 );

    /* configure and submit grid job */

    // logging output
    final String stdOutFileName = m_executable + ".out";
    final String stdErrFileName = m_executable + ".err";

    // for hannover
    final String queue = "dgitest";
    final String factory = "PBS";

    // for kaiserslautern
    // final String queue = "dgitest";
    // final String factory = "SGE"

    final FileObject executableFile = m_workingDir.resolveFile( m_executable );
    if( !executableFile.exists() )
      throw new IOException( "Executable does not exist!" );

    // make file executable
    ((GsiFtpFileObject) executableFile).makeExecutable();

    final GSSCredential credential = getCredential();

    final GDIJobProperties props = new GDIJobProperties();

    props.addPreference( "gdi.targethostname", m_targetHostName );
    props.addPreference( "gdi.executable", "./" + m_executable );
    props.addPreference( "gdi.arguments", m_commandLine );
    props.addPreference( "gdi.environment", m_environment );

    final String sandboxName = m_workingDir.getName().getBaseName();
    props.addPreference( "gdi.sandboxdir", sandboxName );

    props.addPreference( "gdi.stdout", stdOutFileName );
    props.addPreference( "gdi.stderr", stdErrFileName );
    props.addPreference( "gdi.factory", factory );
    props.addPreference( "gdi.queue", queue );
    props.addPreference( "gdi.credential", credential );

    final GDIJob job = GDIJobFactory.createGDIJob( GDIJobFactory.defaultGDIJob );
    job.setProps( props );

    // m_monitor.subTask( String.format( "Submitting grid job to server %s.", m_targetHostName ) );

    final PrintWriter stdPrinter = new PrintWriter( stdOut );
    final PrintWriter errPrinter = new PrintWriter( stdErr );

    // on error iRetVal will be IStatus.ERROR (4)
    // if finished successfully, iRetVal will be IStatus.OK (0)
    int iRetVal = IStatus.ERROR;
    try
    {
      // register observer
      ((GDIObserverSubject) job).registerObserver( this );

      // submit job
      job.submit();

      final long startTime = System.currentTimeMillis();
      finished: while( !isCanceled( cancelable ) )
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
          case UNSUBMITTED:
          case STAGEIN:
          case STAGEOUT:
          case RUNNING:
            Thread.sleep( 1000 );
            break;
          case FINISHED:
            iRetVal = IStatus.OK;
          case FAILED:
            break finished;
        }
      }
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( stdPrinter );
      IOUtils.closeQuietly( errPrinter );

      // clean up job in case of an error
      // this will have no effect if job has finished successfully
      if( job != null && job.getStatus() == GDIState.RUNNING )
      {
        job.cancelJob();
      }

      if( m_manager != null )
        m_manager.close();

      if( isCanceled( cancelable ) )
      {
        errPrinter.println( "Job has been canceled." );
        throw new OperationCanceledException();
      }
    }

    return iRetVal;
  }

  private FileObject createSandbox( final String tempDirName ) throws FileSystemException
  {
    final String gridFtpRoot = "gridftp://" + m_targetHostName;
    final FileObject remoteRoot = m_manager.resolveFile( gridFtpRoot );
    final FileSystem fileSystem = remoteRoot.getFileSystem();

    // get home directory of user who created this job
    final String homeDirString = (String) fileSystem.getAttribute( "HOME_DIRECTORY" );
    final FileObject homeDir = remoteRoot.resolveFile( homeDirString );
    final FileObject workingDir = homeDir.resolveFile( tempDirName );
    workingDir.createFolder();
    return workingDir;
  }

  private GSSCredential getCredential( ) throws IOException
  {
    // 1.
    // search for credential in current security context
    final Subject currentSubject = org.globus.gsi.jaas.JaasSubject.getCurrentSubject();
    if( currentSubject != null )
    {
      final Set<Object> creds = currentSubject.getPrivateCredentials();
      if( creds.size() >= 1 )
      {
        return (GSSCredential) creds.iterator().next();
      }
      else
      {
        // if we are on server side, we cannot just create a credential, so throw an exception
        throw new IOException( "Current subject does not have private credentials." );
      }
    }

    // 2.
    // Authenticate with user credential defined in <USER_HOME>/.globus/cog.properties
    // or (if undefined) use default proxy certificate <TEMP>/X509...
    final ExtendedGSSManager manager = (ExtendedGSSManager) ExtendedGSSManager.getInstance();
    try
    {
      return manager.createCredential( GSSCredential.INITIATE_AND_ACCEPT );
    }
    catch( final GSSException e )
    {
      // wrap as IOException because we cannot do I/O without security
      throw new IOException( e );
    }
  }

  private boolean isCanceled( final ICancelable cancelable )
  {
    return cancelable != null && cancelable.isCanceled();
    // return m_monitor.isCanceled() || (cancelable != null && cancelable.isCanceled());
  }

  /*
   * (non-Javadoc)
   * @see de.unihannover.rvs.gdi.jobsubmit.interfaces.GDIObserver#update(org.gridlab .gat.resources.Job.JobState)
   */
  public void update( final GDIState newStatus )
  {
    m_status = newStatus;
    // m_monitor.subTask( m_status.toString() );
  }
}
