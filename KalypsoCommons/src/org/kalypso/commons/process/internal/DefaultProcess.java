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
package org.kalypso.commons.process.internal;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.process.IProcess;
import org.kalypso.commons.process.ProcessTimeoutException;
import org.kalypso.contribs.java.lang.ICancelable;

/**
 * This implementation executes processes the 'normal' way, i.e. locally using {@link ProcessBuilder}.
 * 
 * @author Gernot Belger
 */
public class DefaultProcess implements IProcess
{
  private final ProcessBuilder m_processBuilder;

  /** will be shown in case of error */
  private final String m_commandLabel;

  private long m_timeout;

  private final File m_command;

  private final URL m_exeUrl;

  public DefaultProcess( final File workingDir, final URL exeUrl, final String[] commandlineArgs )
  {
    Assert.isNotNull( workingDir );
    Assert.isNotNull( exeUrl );

    m_exeUrl = exeUrl;
    m_commandLabel = exeUrl.toString();
    m_command = findCommand( workingDir, exeUrl );

    final List<String> commandLine = new ArrayList<String>();
    commandLine.add( m_command.getAbsolutePath() );
    if( commandlineArgs != null )
    {
      for( final String arg : commandlineArgs )
        commandLine.add( arg );
    }

    m_processBuilder = new ProcessBuilder( commandLine );

    m_processBuilder.directory( workingDir );
  }

  /** Final because called from constructor. */
  private final File findCommand( final File workingDir, final URL exeUrl )
  {
    // If it is a local file we will directly call it.
    final File localExeFile = FileUtils.toFile( exeUrl );
    if( localExeFile != null )
      return localExeFile;

    // Else we will copy the exe to the working directory
    final String path = exeUrl.getPath();
    final String name = FileUtilities.nameFromPath( path );
    final File exeFile = new File( workingDir, name );
    if( exeFile.exists() )
      // TODO: better exception
      throw new IllegalArgumentException( "File with same name already exists in working directory" );
    return exeFile;
  }

  /**
   * @see org.kalypso.commons.process.IProcess#environment()
   */
  @Override
  public Map<String, String> environment( )
  {
    return m_processBuilder.environment();
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
  public int startProcess( final OutputStream stdOut, final OutputStream stdErr, final InputStream stdIn, final ICancelable cancelable ) throws IOException, ProcessTimeoutException
  {
    /* First, copy the exe to a local place if necessary */
    if( !m_command.exists() )
      FileUtilities.makeFileFromUrl( m_exeUrl, m_command, false );

    /* Now start the process in the workDir */
    Process process = null;
    int iRetVal = -1;
    InputStream outStream = null;
    InputStream errStream = null;
    OutputStream inStream = null;
    ProcessControlJob procCtrlThread = null;

    try
    {
      process = m_processBuilder.start();

      procCtrlThread = new ProcessControlJob( process, cancelable, m_timeout );
      procCtrlThread.setSystem( true );
      procCtrlThread.schedule();

      outStream = new BufferedInputStream( process.getInputStream() );
      errStream = new BufferedInputStream( process.getErrorStream() );
      inStream = new BufferedOutputStream( process.getOutputStream() );

      new StreamStreamer( outStream, stdOut );
      new StreamStreamer( errStream, stdErr );
      new StreamStreamer( stdIn, inStream );

      iRetVal = process.waitFor();

      if( procCtrlThread != null )
        procCtrlThread.cancel();
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( inStream );
      IOUtils.closeQuietly( errStream );
      IOUtils.closeQuietly( outStream );
    }

    if( cancelable.isCanceled() )
      throw new OperationCanceledException();

    if( procCtrlThread != null )
    {
      final IStatus result = procCtrlThread.getResult();
      if( result != null && result.matches( IStatus.ERROR ) )
        throw new ProcessTimeoutException( "Timeout bei der Abarbeitung von '" + m_commandLabel + "'" );
    }

    return iRetVal;
  }

  /**
   * @see org.kalypso.commons.process.IProcess#setProgressMonitor(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void setProgressMonitor( final IProgressMonitor monitor )
  {
    // ignore at this time
  }

}
