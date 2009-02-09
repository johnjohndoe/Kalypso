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

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.vfs.FileName;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystem;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.FileSystemManager;
import org.apache.commons.vfs.FileType;
import org.apache.commons.vfs.Selectors;
import org.apache.commons.vfs.provider.gsiftp.GsiFtpFileProvider;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.globus.wsrf.container.Activator;
import org.kalypso.commons.Debug;
import org.kalypso.commons.io.VFSUtilities;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.process.IProcess;
import org.kalypso.commons.process.ProcessTimeoutException;
import org.kalypso.contribs.java.lang.ICancelable;
import org.osgi.framework.Bundle;

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
  private static final String GRIDFTP_SERVER_ROOT = "gridftp://" + GridProcessFactory.GRID_SERVER_URL;

  private static final String SANDBOX_DIR_REPLACEMENT = "/$SANDBOXDIR/";

  private static final int TOTAL_WORK = 10;

  private final File m_workingDir;

  private final URL m_exeUrl;

  private final List<URL> m_inputs;

  private final List<String> m_outputs;

  private final List<String> m_commandLine = new ArrayList<String>();

  private GDIJob m_job = GDIJobFactory.createGDIJob( GDIJobFactory.defaultGDIJob );

  private final String m_sandboxRoot = "gaja3d-" + System.currentTimeMillis();

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
  public SimpleGridProcess( final File workingDir, final URL exeUrl, final String... commandlineArgs )
  {
    Assert.isNotNull( workingDir );
    Assert.isNotNull( exeUrl );

    m_workingDir = workingDir;
    m_exeUrl = exeUrl;
    m_inputs = new ArrayList<URL>();
    m_outputs = new ArrayList<String>();

    if( commandlineArgs != null )
    {
      for( final String arg : commandlineArgs )
        m_commandLine.add( arg );
    }

    try
    {
      final Bundle globusBundle = Activator.getContext().getBundle();
      final File globusDir = FileLocator.getBundleFile( globusBundle );
      if( globusDir.isDirectory() )
      {
        System.setProperty( "GLOBUS_LOCATION", globusDir.getAbsolutePath() );
      }
    }
    catch( final IOException e )
    {
      e.printStackTrace();
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

  public void addInput( final URL input )
  {
    m_inputs.add( input );
  }

  public void addOutput( final String output )
  {
    m_outputs.add( output );
  }

  /**
   * @see org.kalypso.commons.process.IProcess#startProcess(java.io.OutputStream, java.io.OutputStream,
   *      java.io.InputStream, org.kalypso.contribs.java.lang.ICancelable)
   */
  @Override
  public int startProcess( final OutputStream stdOut, final OutputStream stdErr, final InputStream stdIn, final ICancelable cancelable ) throws ProcessTimeoutException, IOException
  {
    // try to convert bundle resouce url to local file
    final String exeUrlName = getUrlAsString( m_exeUrl );

    // initialize simulation monitor if not set
    m_monitor = SubMonitor.convert( m_monitor, String.format( "Running grid process for executable %s in directoy %s.", exeUrlName, m_workingDir.getAbsolutePath() ), TOTAL_WORK );

    // get manager that can handle gsiftp connections
    final FileSystemManager manager = VFSProvidersExtension.getManager();

    // stage-in 3 files: all zipped input files, the executable and the
    // run-script
    final ArrayList<GDIFileTransfer> stageInFiles = new ArrayList<GDIFileTransfer>();
    for( final URL externalInput : m_inputs )
    {
      // try to convert bundle resouce url to local file
      final String externalForm = getUrlAsString( externalInput );
      final String inputBaseName = FileUtilities.nameFromPath( externalForm );
      stageInFiles.add( new GDIFileTransfer( externalForm, GRIDFTP_SERVER_ROOT + SANDBOX_DIR_REPLACEMENT + inputBaseName ) );
    }

    // stage-in the exe to remote place
    final String exeBaseName = FileUtilities.nameFromPath( exeUrlName );
    stageInFiles.add( new GDIFileTransfer( exeUrlName, GRIDFTP_SERVER_ROOT + SANDBOX_DIR_REPLACEMENT + exeBaseName ) );

    /* configure and submit grid job */

    // and logging output
    final String stdOutFileName = exeBaseName + ".out";
    final String stdErrFileName = exeBaseName + ".err";

    final GDIJobProperties props = new GDIJobProperties();
    props.addPreference( "gdi.targethostname", GridProcessFactory.GRID_SERVER_URL );
    props.addPreference( "gdi.executable", "./" + exeBaseName );
    props.addPreference( "gdi.arguments", m_commandLine );
    props.addPreference( "gdi.stdout", stdOutFileName );
    props.addPreference( "gdi.stderr", stdErrFileName );
    props.addPreference( "gdi.filestagein", stageInFiles );

    // no stage-out, we will handle it ourselves
    final ArrayList<GDIFileTransfer> noStageOut = new ArrayList<GDIFileTransfer>();
    props.addPreference( "gdi.filestageout", noStageOut );
    props.addPreference( "gdi.queue", "dgitest" ); // for hannover
    props.addPreference( "gdi.factory", "Fork" ); // for hannover
    // props.addPreference( "gdi.queue", "SGE" ); // for kaiserslautern

    props.addPreference( "gdi.sandboxdir", m_sandboxRoot );
    props.addPreference( "gdi.environment", m_environment );
    m_job.setProps( props );

    m_monitor.subTask( String.format( "Submitting grid job to server %s.", GridProcessFactory.GRID_SERVER_URL ) );

    final PrintWriter errorPrinter = new PrintWriter( stdErr );

    // register observer
    ((GDIObserverSubject) m_job).registerObserver( this );
    int returnCode = Status.ERROR;
    FileObject homeDir = null;
    try
    {
      // find home directory on server, this may fail if no credentials are provided
      homeDir = getServerHomeDir( manager );
      final FileObject localWorking = manager.toFileObject( m_workingDir );
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
          case UNSUBMITTED:
          case RUNNING:
          case STAGEIN:
          case STAGEOUT:
            sleep();
            break;
          case FINISHED:
            // update local directory
            stageOutAndCleanUp( homeDir, localWorking );
            localWorking.refresh();
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
      e.printStackTrace( errorPrinter );
    }
    finally
    {
      // close connections to grid ftp server so vfs references can be
      // cleaned up
      if( homeDir != null )
        manager.closeFileSystem( homeDir.getFileSystem() );

      // clean up job in case of an error
      // this will have no effect if job has finished successfully
      if( m_job != null && m_job.getStatus() == GDIState.RUNNING )
        m_job.cancelJob();
      m_job = null;

      if( m_monitor.isCanceled() || (cancelable != null && cancelable.isCanceled()) )
      {
        errorPrinter.println( "Job has been canceled." );
        throw new OperationCanceledException();
      }
    }

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

  private String getUrlAsString( final URL url ) throws IOException
  {
    URL exeUrl = FileLocator.toFileURL( url );
    if( exeUrl == null )
    {
      // if this did not work, try to use original url
      exeUrl = url;
    }
    final String exeUrlName = exeUrl.toExternalForm();
    return exeUrlName;
  }

  private FileObject getServerHomeDir( final FileSystemManager manager ) throws FileSystemException
  {
    final FileObject remoteRoot = manager.resolveFile( GRIDFTP_SERVER_ROOT );
    final FileSystem fileSystem = remoteRoot.getFileSystem();
    final String homeDirString = (String) fileSystem.getAttribute( GsiFtpFileProvider.ATTR_HOME_DIR );
    final FileObject homeDir = remoteRoot.resolveFile( homeDirString );
    return homeDir;
  }

  private void stageOutAndCleanUp( final FileObject homeDir, final FileObject localWorking ) throws IOException
  {
    final FileObject remoteWorking = homeDir.resolveFile( m_sandboxRoot );
    remoteWorking.refresh();

    final FileObject[] children = remoteWorking.getChildren();
    nextChild: for( FileObject child : children )
    {
      final FileName childName = child.getName();
      final String baseName = childName.getBaseName();
      for( final String output : m_outputs )
      {
        if( FilenameUtils.wildcardMatch( baseName, output ) )
        {
          final FileObject destination = localWorking.resolveFile( baseName );
          if( FileType.FILE.equals( child.getType() ) )
          {
            /* Copy ... */
            VFSUtilities.copyFileTo( child, destination, true );
          }
          else if( FileType.FOLDER.equals( child.getType() ) )
          {
            /* Copy ... */
            Debug.println( "Copy directory " + childName + " to " + destination.getName() + " ..." );
            VFSUtilities.copyDirectoryToDirectory( child, destination, true );
          }
          else
          {
            Debug.println( "Could not determine the file type ..." );
          }
          continue nextChild;
        }
      }
    }

    remoteWorking.delete( Selectors.SELECT_ALL );
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
