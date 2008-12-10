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
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystem;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.FileSystemManager;
import org.apache.commons.vfs.Selectors;
import org.apache.commons.vfs.provider.gsiftp.GsiFtpFileProvider;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.gridlab.gat.resources.Job.JobState;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.commons.process.IProcess;
import org.kalypso.commons.process.ProcessTimeoutException;
import org.kalypso.contribs.java.lang.ICancelable;

import de.unihannover.rvs.gdi.jobsubmit.impl.GDIFileTransfer;
import de.unihannover.rvs.gdi.jobsubmit.impl.GDIJobFactory;
import de.unihannover.rvs.gdi.jobsubmit.interfaces.GDIJob;
import de.unihannover.rvs.gdi.jobsubmit.interfaces.GDIObserver;
import de.unihannover.rvs.gdi.jobsubmit.interfaces.GDIObserverSubject;

/**
 * This implementation executes processes in a computing grid.
 * 
 * @author Stefan Kurzbach
 */
public class GridProcess implements IProcess, GDIObserver
{

  private static final int TOTAL_WORK = 10;

  private final File m_workingDir;

  private final URL m_exeUrl;

  private final List<URL> m_externalInputs;

  private final List<String> m_commandLine = new ArrayList<String>();

  private IProgressMonitor m_monitor;

  private JobState m_status = JobState.INITIAL;

  protected GDIJob m_job = GDIJobFactory.createGDIJob( GDIJobFactory.defaultGDIJob );

  /*
   * for adaptation of environment
   */
  private Map<String, String> m_environment = new Map<String, String>()
  {

    @Override
    public void clear( )
    {
      m_job.getEnvironment().clear();
    }

    @Override
    public boolean containsKey( Object key )
    {
      return m_job.getEnvironment().containsKey( key );
    }

    @Override
    public boolean containsValue( Object value )
    {
      return m_job.getEnvironment().containsValue( value );
    }

    @Override
    public Set<java.util.Map.Entry<String, String>> entrySet( )
    {
      throw new UnsupportedOperationException();
    }

    @Override
    public String get( Object key )
    {
      return (String) m_job.getEnvironment().get( key );
    }

    @Override
    public boolean isEmpty( )
    {
      return m_job.getEnvironment().isEmpty();
    }

    @Override
    public Set<String> keySet( )
    {
      return m_job.getEnvironment().keySet();
    }

    @Override
    public String put( String key, String value )
    {
      return (String) m_job.getEnvironment().put( key, value );
    }

    @Override
    public void putAll( Map< ? extends String, ? extends String> m )
    {
      m_job.getEnvironment().putAll( m );
    }

    @Override
    public String remove( Object key )
    {
      return (String) m_job.getEnvironment().remove( key );
    }

    @Override
    public int size( )
    {
      return m_job.getEnvironment().size();
    }

    @Override
    public Collection<String> values( )
    {
      throw new UnsupportedOperationException();
    }
  };

  private long m_timeout;

  private String m_sandboxRoot;

  /**
   * Creates a new grid process that gets its files from workingDir and runs the executable exeUrl on a computing grid
   * server as an asynchronous job
   * 
   * @param workingDir
   * @param exeUrl
   * @param commandlineArgs
   */
  public GridProcess( final File workingDir, final URL exeUrl, final String... commandlineArgs )
  {
    Assert.isNotNull( workingDir );
    Assert.isNotNull( exeUrl );

    m_workingDir = workingDir;
    m_exeUrl = exeUrl;
    m_externalInputs = new ArrayList<URL>();

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

  public void addExternalInput( final URL input )
  {
    m_externalInputs.add( input );
  }

  public void addExternalInputs( final List<URL> inputs )
  {
    m_externalInputs.addAll( inputs );
  }

  /**
   * @see org.kalypso.commons.process.IProcess#startProcess(java.io.OutputStream, java.io.OutputStream,
   *      java.io.InputStream, org.kalypso.contribs.java.lang.ICancelable)
   */
  @Override
  public int startProcess( final OutputStream stdOut, final OutputStream stdErr, final InputStream stdIn, final ICancelable cancelable ) throws IOException, ProcessTimeoutException
  {
    // try to convert bundle resouce url to local file
    final String exeUrlName = getUrlAsString( m_exeUrl );

    // initialize simulation monitor if not set
    m_monitor = SubMonitor.convert( m_monitor, String.format( "Running grid process for executable %s in directoy %s.", exeUrlName, m_workingDir.getAbsolutePath() ), TOTAL_WORK );

    // get manager that can handle gsiftp connections
    final FileSystemManager manager = VFSProvidersExtension.getManager();

    // stage-in 3 files: all zipped input files, the executable and the
    // run-script
    final FileObject localWorking = manager.toFileObject( m_workingDir );
    final ArrayList<GDIFileTransfer> stageInFiles = new ArrayList<GDIFileTransfer>();

    for( final URL externalInput : m_externalInputs )
    {
      // try to convert bundle resouce url to local file
      final String externalForm = getUrlAsString( externalInput );
      final String inputBaseName = FileUtilities.nameFromPath( externalForm );
      stageInFiles.add( new GDIFileTransfer( externalForm, inputBaseName ) );
    }
    // stage-in the exe to remote place
    final String exeBaseName = FileUtilities.nameFromPath( exeUrlName );
    stageInFiles.add( new GDIFileTransfer( exeUrlName, exeBaseName ) );

    // stage-in the script to run with redirected stdin and stdout
    final String runScript = "run.sh";
    URL m_scriptUrl = getClass().getResource( runScript );
    m_scriptUrl = FileLocator.toFileURL( m_scriptUrl );
    stageInFiles.add( new GDIFileTransfer( m_scriptUrl.toExternalForm(), runScript ) );

    // zip all files and stage-in
    final String zipSuffix = ".zip";
    final String zipFileName = exeBaseName + zipSuffix;
    final File zipFile = File.createTempFile( exeBaseName, zipSuffix );
    zipFile.deleteOnExit();
    final File[] filesInWorking = m_workingDir.listFiles();
    if( filesInWorking.length > 0 )
    {
      ZipUtilities.zip( zipFile, m_workingDir );
      stageInFiles.add( new GDIFileTransfer( zipFile.toURI().toURL().toExternalForm(), zipFileName ) );
    }

    /* configure and submit grid job */

    // set the environment variable GRID_PROCESS (used by run.sh)
    environment().put( "GRID_PROCESS", exeBaseName );

    // server url for job submission
    final String gridServerUrl = GridProcessFactory.GRID_SERVER_URL;

    // setup logging output
    final String stdOutFileName = exeBaseName + ".out";
    final String stdErrFileName = exeBaseName + ".err";
    final String stdInFileName = null; // no stdin

    m_job.setTargetHostName( gridServerUrl );
    m_job.setExecutable( "./" + runScript ); // run the script
    m_job.setArguments( m_commandLine );
    m_job.setStdOut( stdOutFileName );
    m_job.setStdErr( stdErrFileName );
    m_job.setStdIn( stdInFileName );
    m_job.setFileStageIn( stageInFiles );
    // no stage-out, we will handle it ourselves
    final ArrayList<GDIFileTransfer> noStageOut = new ArrayList<GDIFileTransfer>();
    m_job.setFileStageOut( noStageOut );
    m_job.setQueue( "dgitest" ); // for hannover
    // m_job.setFactory( "SGE" ); // for kaiserslautern

    m_monitor.subTask( String.format( "Submitting grid job to server %s.", gridServerUrl ) );

    // find home directory on server
    final FileObject homeDir = getServerHomeDir( manager, gridServerUrl );
    // set up streamers for stdout and stderr
    final StreamStreamer stdOutStream = new StreamStreamer( stdOut );
    final StreamStreamer stdErrStream = new StreamStreamer( stdErr );

    final PrintWriter errorPrinter = new PrintWriter( stdErr );

    // register observer
    ((GDIObserverSubject) m_job).registerObserver( this );
    int returnCode = Status.ERROR;
    try
    {
      m_job.submit();
      m_sandboxRoot = m_job.getSandboxRoot();

      // wait for FINISHED or FAILED status
      stdOutStream.start();
      stdErrStream.start();

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
        next: switch( m_status )
        {
          case INITIAL:
          case SCHEDULED:
          case PRE_STAGING:
          case ON_HOLD:
          case POST_STAGING:
            break next;
          case STOPPED:
          case UNKNOWN:
          case SUBMISSION_ERROR:
            break finished;
          case RUNNING:
            // this does not work right, for some reason the streams
            // only
            // deliver data when the process has finished
            // stdOutStream.setInputStream(m_job.getStdout());
            // stdErrStream.setInputStream(m_job.getStderr());
            updateStreamer( stdOutFileName, stdOutStream );
            updateStreamer( stdErrFileName, stdErrStream );
            // updateSandboxToLocal( homeDir, localWorking, false );
        }
        sleep();
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace( errorPrinter );
    }
    finally
    {
      // last update of local directory
      updateSandboxToLocal( homeDir, localWorking, true );
      localWorking.refresh();

      // last update of streams
      updateStreamer( stdOutFileName, stdOutStream );
      updateStreamer( stdErrFileName, stdErrStream );
      stdOutStream.finish();
      stdErrStream.finish();

      // set return code
      switch( m_status )
      {
        case INITIAL:
        case SCHEDULED:
        case PRE_STAGING:
        case RUNNING:
        case ON_HOLD:
        case POST_STAGING:
        case STOPPED:
          returnCode = IStatus.OK;
          break;
        case UNKNOWN:
          errorPrinter.println( "ERROR: unknown job status. Maybe a network problem?" );
          returnCode = IStatus.ERROR;
          break;
        case SUBMISSION_ERROR:
          errorPrinter.println( "Job submission error." );
          returnCode = IStatus.ERROR;
          break;
      }

      // delete zip file (even before exiting)
      zipFile.delete();

      // close connections to grid ftp server so vfs references can be
      // cleaned up
      manager.closeFileSystem( homeDir.getFileSystem() );

      // clean up job in case of an error
      // this will have no effect if job has finished successfully
      m_job.cancelJob();
      m_job = null;

      if( m_monitor.isCanceled() || (cancelable != null && cancelable.isCanceled()) )
      {
        errorPrinter.println( "Job has been canceled." );
        returnCode = IStatus.CANCEL;
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

  private void updateStreamer( final String stdOutFileName, final StreamStreamer stdOutStream )
  {
    try
    {
      final FileInputStream stdout = new FileInputStream( new File( m_workingDir, stdOutFileName ) );
      stdOutStream.setInputStream( stdout );
    }
    catch( final FileNotFoundException e )
    {
      // ignore
    }
  }

  private FileObject getServerHomeDir( final FileSystemManager manager, final String gridServerUrl ) throws FileSystemException
  {
    final FileObject remoteRoot = manager.resolveFile( "gsiftp://" + gridServerUrl );
    final FileSystem fileSystem = remoteRoot.getFileSystem();
    final String homeDirString = (String) fileSystem.getAttribute( GsiFtpFileProvider.ATTR_HOME_DIR );
    final FileObject homeDir = remoteRoot.resolveFile( homeDirString );
    return homeDir;
  }

  private void updateSandboxToLocal( final FileObject homeDir, final FileObject localWorking, final boolean delete )
  {
    if( m_sandboxRoot != null )
    {
      // try to synchronize files
      FileSynchronizer fileSynchronizer;
      try
      {
        final FileObject remoteWorking = homeDir.resolveFile( m_sandboxRoot );
        remoteWorking.refresh();
        fileSynchronizer = new FileSynchronizer( localWorking, remoteWorking );
        fileSynchronizer.updateLocal();
        if( delete )
        {
          remoteWorking.delete( Selectors.SELECT_ALL );
        }
      }
      catch( final IOException e )
      {
        e.printStackTrace();
      }
    }
  }

  /*
   * (non-Javadoc)
   * @see de.unihannover.rvs.gdi.jobsubmit.interfaces.GDIObserver#update(org.gridlab .gat.resources.Job.JobState)
   */
  public void update( final JobState newStatus )
  {
    m_status = newStatus;
    m_monitor.subTask( m_status.toString() );
  }

}
