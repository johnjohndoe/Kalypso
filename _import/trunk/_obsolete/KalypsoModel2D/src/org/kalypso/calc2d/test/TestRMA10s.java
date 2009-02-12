/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.calc2d.test;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.lang.ProcessHelper.ProcessControlThread;
import org.kalypso.commons.java.lang.ProcessHelper.ProcessTimeoutException;
import org.kalypso.commons.java.lang.ProcessHelper.StreamStreamer;
import org.kalypso.contribs.java.lang.ICancelable;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.SimulationException;

/**
 * @author antanas
 *
 */
public class TestRMA10s
{

  public static void main( String[] args ) throws SimulationException
  {
    final ISimulationMonitor monitor = null;
    final String path = "D:\\working";
    final String executable = "D:\\working\\start.bat";
    long timeOut = 1000l * 60l * 60l; // max 60 minutes

    final File exeDir = new File(path);
    FileOutputStream logOS = null;
    FileOutputStream errorOS = null;
    try
    {
      logOS = new FileOutputStream( new File( path, "exe.log" ) );
      errorOS = new FileOutputStream( new File( path, "exe.err" ) );
      startProcess( executable, new String[0], exeDir, monitor, timeOut, logOS, errorOS, null );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new SimulationException( "SimulationException", e );
    }
    finally
    {
      IOUtils.closeQuietly( logOS );
      IOUtils.closeQuietly( errorOS );
    }
  }
  
  public static int startProcess( final String sCmd, final String[] envp, final File fleExeDir, final ICancelable cancelable, final long lTimeOut, final OutputStream wLog, final OutputStream wErr, final InputStream rIn ) throws IOException, ProcessTimeoutException
  {
    final Process process;
    int iRetVal = -1;
    InputStream inStream = null;
    InputStream errStream = null;
    OutputStream outStream = null;
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
          System.out.println("Process not finished! Exception trace:");
          e.printStackTrace();
        }

        if( cancelable!=null && cancelable.isCanceled() )
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
      System.out.println("Process not finished! Exception trace:");
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
      throw new ProcessTimeoutException( "Process timed out: '" + sCmd + "'" );
    }
    return iRetVal;
  }
  

}
