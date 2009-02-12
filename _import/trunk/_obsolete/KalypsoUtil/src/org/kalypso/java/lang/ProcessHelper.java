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
package org.kalypso.java.lang;

import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Writer;

import org.apache.commons.io.CopyUtils;

/**
 * 
 * @author Thül
 */
public class ProcessHelper
{

  public ProcessHelper()
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
   * @param wLog
   * @param wErr
   * 
   * @throws IOException
   * @throws ProcessTimeoutException
   */
  public static int startProcess( final String sCmd, final String[] envp, final File fleExeDir,
      final ICancelable cancelable, final long lTimeOut, final Writer wLog, final Writer wErr ) throws IOException,
      ProcessTimeoutException
  {
    final Process process;
    int iRetVal = -1;
    InputStreamReader inStreamRdr = null;
    InputStreamReader errStreamRdr = null;
    ProcessControlThread procCtrlThread = null;

    try
    {
      process = Runtime.getRuntime().exec( sCmd, envp, fleExeDir );
      if( lTimeOut > 0 )
      {
        procCtrlThread = new ProcessControlThread( process, lTimeOut );
        procCtrlThread.start();
      }

      inStreamRdr = new InputStreamReader( process.getInputStream() );
      errStreamRdr = new InputStreamReader( process.getErrorStream() );
      while( true )
      {
        CopyUtils.copy( inStreamRdr, wLog );
        CopyUtils.copy( errStreamRdr, wErr );

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

    public void run()
    {
      synchronized( this )
      {
        try
        {
          m_bProcCtrlActive = true;
          wait( m_lTimeout );
        }
        catch( InterruptedException ex )
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

    public synchronized void endProcessControl()
    {
      // stoppt die Überwachung des Prozesses
      m_bProcCtrlActive = false;
      notifyAll();
    }

    public boolean procDestroyed()
    {
      // wurde der Prozess durch diesen Thread abbgebrochen?
      return m_bProcDestroyed;
    }
  }

  public static class ProcessTimeoutException extends Exception
  {
    public ProcessTimeoutException()
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