/**
 * ---------------- FILE HEADER KALYPSO ------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestraße 22 21073
 * Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: g.belger@bjoernsen.de m.schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------
 */
package org.kalypso.commons.java.lang;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.lang.ICancelable;

/**
 * @author Thül
 */
public class ProcessHelper
{
  public static class StreamStreamer extends Thread
  {
    private final OutputStream m_os;

    private final InputStream m_is;

    public StreamStreamer( final InputStream is, final OutputStream os )
    {
      m_os = os;
      m_is = is;

      start();
    }

    @Override
    public void run( )
    {
      if( m_is == null )
        return;

      try
      {
        final byte[] stuff = new byte[40];

        while( true )
        {
          final int read = m_is.read( stuff );
          if( read == -1 )
            break;

          // System.out.print( new String( stuff, 0, read ) );

          if( m_os != null )
            m_os.write( stuff, 0, read );
        }
      }
      catch( final IOException ioe )
      {
        ioe.printStackTrace();
      }
    }
  }

  public ProcessHelper( )
  {
    // wird nicht instantiiert
  }

  /**
   * startet Prozess (sCmd, envp, fleExeDir), schreibt Ausgaben nach wLog, wErr, beendet den Prozess automatisch nach
   * iTOut ms (iTOut = 0 bedeutet, dass der Prozess nicht abgebrochen wird), die Abarbeitung des Prozesses beachtet auch
   * den Cancel-Status von cancelable
   * 
   * @param sCmd
   * @param envp
   * @param fleExeDir
   * @param cancelable
   * @param lTimeOut
   *            Time-out in milliseconds
   * @param wLog
   * @param wErr
   * @throws IOException
   * @throws ProcessTimeoutException
   * @deprecated use
   *             {@link #startProcess(String, String[], File, ICancelable, long, OutputStream, OutputStream, InputStream)
   */
  @Deprecated
  public static int startProcess( final String sCmd, final String[] envp, final File fleExeDir, final ICancelable cancelable, final long lTimeOut, final Writer wLog, final Writer wErr ) throws IOException, ProcessTimeoutException
  {
    final Process process;
    int iRetVal = -1;
    InputStreamReader inStreamRdr = null;
    InputStreamReader errStreamRdr = null;
    OutputStreamWriter outStreamWtr = null;
    ProcessControlThread procCtrlThread = null;

    try
    {
      process = Runtime.getRuntime().exec( sCmd, envp, fleExeDir );

      if( lTimeOut > 0 )
      {
        procCtrlThread = new ProcessControlThread( process, lTimeOut );
        procCtrlThread.start();
      }

      outStreamWtr = new OutputStreamWriter( new BufferedOutputStream( process.getOutputStream() ) );

      inStreamRdr = new InputStreamReader( /* new BufferedInputStream( */process.getInputStream() )/* ) */;
      errStreamRdr = new InputStreamReader( new BufferedInputStream( process.getErrorStream() ) );
      while( true )
      {
        IOUtils.copy( inStreamRdr, wLog );
        IOUtils.copy( errStreamRdr, wErr );

        try
        {
          iRetVal = process.exitValue();
          break;
        }
        catch( final IllegalThreadStateException e )
        {
          // Prozess noch nicht fertig, weiterlaufen lassen
        }

        if( cancelable.isCanceled() )
        {
          process.destroy();
          if( procCtrlThread != null )
          {
            procCtrlThread.endProcessControl();
          }
          iRetVal = process.exitValue();
          return iRetVal;
        }
        Thread.sleep( 100 );
      }
      if( procCtrlThread != null )
      {
        procCtrlThread.endProcessControl();
      }
    }
    catch( final InterruptedException e )
    {
      // kann aber eigentlich gar nicht passieren
      // (wird geworfen von Thread.sleep( 100 ))
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( inStreamRdr );
      IOUtils.closeQuietly( errStreamRdr );
      IOUtils.closeQuietly( outStreamWtr );
    }

    if( procCtrlThread != null && procCtrlThread.procDestroyed() )
    {
      throw new ProcessTimeoutException( "Timeout bei der Abarbeitung von '" + sCmd + "'" );
    }
    return iRetVal;
  }

  /**
   * Same as
   * {@link #startProcess(String, String[], File, ICancelable, long, OutputStream, OutputStream, InputStream, 100, null)}
   */
  public static int startProcess( final String sCmd, final String[] envp, final File fleExeDir, final ICancelable cancelable, final long lTimeOut, final OutputStream wLog, final OutputStream wErr, final InputStream rIn ) throws IOException, ProcessTimeoutException
  {
    return startProcess( sCmd, envp, fleExeDir, cancelable, lTimeOut, wLog, wErr, rIn, 100, null );
  }

  /**
   * startet Prozess (sCmd, envp, fleExeDir), schreibt Ausgaben nach wLog, wErr, beendet den Prozess automatisch nach
   * iTOut ms (iTOut = 0 bedeutet, dass der Prozess nicht abgebrochen wird), die Abarbeitung des Prozesses beachtet auch
   * den Cancel-Status von cancelable
   * 
   * @param sCmd
   * @param envp
   * @param fleExeDir
   * @param cancelable
   * @param lTimeOut
   *            Time-out in milliseconds
   * @param wLog
   *            Gets connected to the output stream of the process.
   * @param wErr
   *            Gets connected to the error stream of the process.
   * @param rIn
   *            Gets connected to the input stream of the process.
   * @param sleepTime
   *            Sleep time for each loop, for checking the running process.
   * @param idleWorker
   *            This runnable gets called for every loop, while checking the running process.
   * @throws IOException
   * @throws ProcessTimeoutException
   */
  public static int startProcess( final String sCmd, final String[] envp, final File fleExeDir, final ICancelable cancelable, final long lTimeOut, final OutputStream wLog, final OutputStream wErr, final InputStream rIn, final int sleepTime, final Runnable idleWorker ) throws IOException, ProcessTimeoutException
  {
    final Process process;
    int iRetVal = -1;
    final InputStream inStream = null;
    final InputStream errStream = null;
    final OutputStream outStream = null;
    ProcessControlThread procCtrlThread = null;

    try
    {
      process = Runtime.getRuntime().exec( sCmd, envp, fleExeDir );

      if( lTimeOut > 0 )
      {
        procCtrlThread = new ProcessControlThread( process, lTimeOut );
        procCtrlThread.start();
      }

      new StreamStreamer( process.getInputStream(), wLog );
      new StreamStreamer( process.getErrorStream(), wErr );
      new StreamStreamer( rIn, process.getOutputStream() );

      while( true )
      {
        try
        {
          iRetVal = process.exitValue();
          break;
        }
        catch( final IllegalThreadStateException e )
        {
          // Prozess noch nicht fertig, weiterlaufen lassen
        }

        // TODO: cancelling the process does not work
        if( cancelable.isCanceled() )
        {
          process.destroy();
          if( procCtrlThread != null )
          {
            procCtrlThread.endProcessControl();
          }
          iRetVal = process.exitValue();
          return iRetVal;
        }

        if( idleWorker != null )
        {
          try
          {
            idleWorker.run();
          }
          catch( final Throwable t )
          {
            t.printStackTrace();
          }
        }

        Thread.sleep( sleepTime );
      }
      if( procCtrlThread != null )
      {
        procCtrlThread.endProcessControl();
      }
    }
    catch( final InterruptedException e )
    {
      // kann aber eigentlich gar nicht passieren
      // (wird geworfen von Thread.sleep( 100 ))
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( inStream );
      IOUtils.closeQuietly( errStream );
      IOUtils.closeQuietly( outStream );
    }

    if( procCtrlThread != null && procCtrlThread.procDestroyed() )
    {
      throw new ProcessTimeoutException( "Timeout bei der Abarbeitung von '" + sCmd + "'" );
    }
    return iRetVal;
  }

  /**
   * Thread, der die Ausführung des Prozesses proc nach lTimeout ms abbricht. bei lTimeout = 0 wird die Ausführung des
   * Prozesses proc nicht abgebrochen.
   * 
   * @author Thül
   */
  public static class ProcessControlThread extends Thread
  {
    private volatile boolean m_bProcCtrlActive = false;

    private volatile boolean m_bProcDestroyed = false;

    private final long m_lTimeout;

    private final Process m_proc;

    public ProcessControlThread( final Process proc, final long lTimeout )
    {
      m_proc = proc;
      m_lTimeout = lTimeout;
    }

    @Override
    public void run( )
    {
      synchronized( this )
      {
        try
        {
          m_bProcCtrlActive = true;
          wait( m_lTimeout );
        }
        catch( final InterruptedException ex )
        {
          // sollte nicht passieren
        }
      }
      if( m_bProcCtrlActive )
      {
        // Prozess läuft nach Ablauf von m_lTimeout ms immer noch: Abbruch
        m_bProcDestroyed = true;
        m_proc.destroy();
      }
    }

    public synchronized void endProcessControl( )
    {
      // stoppt die Überwachung des Prozesses
      m_bProcCtrlActive = false;
      notifyAll();
    }

    public boolean procDestroyed( )
    {
      // wurde der Prozess durch diesen Thread abbgebrochen?
      return m_bProcDestroyed;
    }
  }

  public static class ProcessTimeoutException extends Exception
  {
    public ProcessTimeoutException( )
    {
      super();

    }

    public ProcessTimeoutException( final String message )
    {
      super( message );

    }

    public ProcessTimeoutException( final Throwable cause )
    {
      super( cause );

    }

    public ProcessTimeoutException( final String message, final Throwable cause )
    {
      super( message, cause );

    }
  }
}