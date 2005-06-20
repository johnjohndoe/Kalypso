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

package org.kalypso.lhwsachsenanhalt.tubig;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.util.StringTokenizer;

import org.apache.commons.io.IOUtils;
import org.kalypso.java.lang.ProcessHelper;
import org.kalypso.java.lang.ProcessHelper.ProcessTimeoutException;
import org.kalypso.services.calculation.job.ICalcMonitor;

public class TubigBatchInterpreter
{

  /**
   * Batch-Interpreter <br>
   * Verwendung für die Tubig-Modelle
   * 
   * @throws TubigBatchException
   * 
   * @author Thül
   */
  public static void runBatch( final File fleExeDir, final File fleBatch, final ICalcMonitor cancelable )
      throws TubigBatchException
  {
    final File fleLog;
    final File fleErr;

    fleLog = new File( fleExeDir, TubigConst.NAME_BAT + TubigConst.NAME_EXT_LOG );
    fleErr = new File( fleExeDir, TubigConst.NAME_BAT + TubigConst.NAME_EXT_ERR );
    runBatch( fleExeDir, fleBatch, fleLog, fleErr, cancelable );
  }

  public static void runBatch( final File fleExeDir, final File fleBatch, File fleLog, File fleErr,
      final ICalcMonitor cancelable ) throws TubigBatchException
  {
    final FileOutputStream strmLog;
    final FileOutputStream strmErr;
    PrintWriter pwLog;
    PrintWriter pwErr;

    pwLog = null;
    pwErr = null;
    try
    {
      strmLog = new FileOutputStream( fleLog );
      strmErr = new FileOutputStream( fleErr );
      pwLog = new PrintWriter( new BufferedWriter( new OutputStreamWriter( strmLog, TubigConst.TUBIG_CODEPAGE ) ) );
      pwErr = new PrintWriter( new BufferedWriter( new OutputStreamWriter( strmErr, TubigConst.TUBIG_CODEPAGE ) ) );
      runBatch( fleExeDir, fleBatch, pwLog, pwErr, cancelable );
    }
    catch( final FileNotFoundException e )
    {
      e.printStackTrace();
      throw new TubigBatchException( cancelable, TubigBatchException.STATUS_ERROR, TubigConst.FINISH_ERROR_TEXT );
    }
    catch( final UnsupportedEncodingException e )
    {
      e.printStackTrace();
      throw new TubigBatchException( cancelable, TubigBatchException.STATUS_ERROR, TubigConst.FINISH_ERROR_TEXT );
    }
    finally
    {
      IOUtils.closeQuietly( pwLog );
      IOUtils.closeQuietly( pwErr );
    }
  }

  public static void runBatch( final File fleExeDir, final File fleBatch, final PrintWriter pwLog,
      final PrintWriter pwErr, final ICalcMonitor cancelable ) throws TubigBatchException
  {
    InputStreamReader rdrBatch;

    rdrBatch = null;

    try
    {
      rdrBatch = new FileReader( fleBatch );
      runBatch( fleExeDir, rdrBatch, pwLog, pwErr, cancelable );
    }
    catch( final FileNotFoundException e )
    {
      e.printStackTrace();
      throw new TubigBatchException( cancelable, TubigBatchException.STATUS_ERROR, TubigConst.FINISH_ERROR_TEXT );
    }
    finally
    {
      IOUtils.closeQuietly( rdrBatch );
    }
  }

  public static void runBatch( final File fleExeDir, final Reader rdrBatch, final PrintWriter pwLog,
      final PrintWriter pwErr, final ICalcMonitor cancelable ) throws TubigBatchException
  {
    final LineNumberReader lneNumRdrBatch;
    final String sRegExPath = "\\Q%1\\E";
    StringWriter swInStream;

    String sZeile;
    String sZeileUpper;
    String sCmd;
    StringTokenizer strTok;
    File newDir;

    boolean bExeEnde = false;
    int iTimeout = TubigConst.BAT_TIMEOUT;

    swInStream = new StringWriter();
    lneNumRdrBatch = new LineNumberReader( rdrBatch );

    pwLog.println( TubigConst.MESS_BERECHNUNG_WIRD_GESTARTET );
    try
    {

      sZeile = lneNumRdrBatch.readLine();
      if( cancelable.isCanceled() )
      {
        pwLog.println( TubigConst.MESS_BERECHNUNG_ABGEBROCHEN );
        return;
      }

      while( sZeile != null )
      {
        sZeileUpper = sZeile.toUpperCase();
        if( sZeileUpper.startsWith( "REM" ) )
        {
          // Überlesen, Info in Log
          pwLog.println( sZeile );
        }
        else
        {
          if( sZeileUpper.startsWith( "BREAK" ) || sZeileUpper.startsWith( "HALT" ) || sZeileUpper.startsWith( "ENDE" ) )
          {
            // Überlesen, Info in Log aber kein Abbruch und keine
            // Benutzerinteraktion
            pwLog.println( TubigConst.LOG_COMMENT_UEBERLESEN + sZeile );
          }
          else
          {
            if( sZeileUpper.startsWith( "PAUSE" ) )
            {
              // Überlesen, Info in Log aber keine Benutzerinteraktion...
              pwLog.println( TubigConst.LOG_COMMENT_UEBERLESEN + sZeile );
            }
            else
            {
              if( sZeileUpper.startsWith( "LABEL" ) )
              {
                // Überlesen, Info in Log
                pwLog.println( TubigConst.LOG_COMMENT_UEBERLESEN + sZeile );
              }
              else
              {
                if( sZeileUpper.startsWith( "M_MESS" ) )
                {
                  // Überlesen, Info in Log aber keine Benutzerinteraktion...
                  pwLog.println( TubigConst.LOG_COMMENT_UEBERLESEN + sZeile );
                }
                else
                {
                  if( sZeileUpper.startsWith( "M_LTAB" ) )
                  {
                    // Überlesen, Info in Log
                    pwLog.println( TubigConst.LOG_COMMENT_UEBERLESEN + sZeile );
                  }
                  else
                  {
                    if( sZeileUpper.startsWith( "M_FEHLER" ) )
                    {
                      // Überlesen, Info in Log aber keine Datenprüfung...
                      pwLog.println( TubigConst.LOG_COMMENT_UEBERLESEN + sZeile );
                    }
                    else
                    {
                      final String absolutePath = fleExeDir.getAbsolutePath();
                      if( sZeileUpper.startsWith( "MKDIR" ) )
                      {
                        // Verzeichnis anlegen (in Java, damit kein Konflikt mit
                        // den Sicherheitseinstellungen auftritt)
                        strTok = new StringTokenizer( sZeileUpper );
                        strTok.nextToken();
                        if( strTok.hasMoreTokens() )
                        {
                          newDir = new File( absolutePath, strTok.nextToken() );
                          newDir.mkdirs();
                        }
                      }
                      else
                      {
                        if( sZeileUpper.startsWith( "M_" ) )
                        {
                          sCmd = absolutePath + File.separator + sZeile;
                        }
                        else if( sZeileUpper.startsWith( "BODESTEU" ) )
                        {
                          sCmd = TubigConst.START_IN_CMD + absolutePath + File.separator + sZeile;
                        }
                        else
                        {
                          // '%1' in sZeile durch absoluten Pfad ersetzen
                          // slashes durch doppelte ersetzen, weil replaceAll
                          // einfach alle auftretenden '\\' schluckt...
                          String string = absolutePath.replaceAll( "\\\\", "\\\\\\\\" );
                          sCmd = sZeile.replaceAll( sRegExPath, string );
                        }
                        pwLog.println( sCmd );
                        if( !"".equals( sCmd ) )
                        {
                          bExeEnde = false;
                          // das kann passieren:
                          // TimeOut: wirft ProcessTimeoutException
                          // cancel: wird durch cancelable.isCanceled()weiterverarbeitet
                          // normal fertig: RückgabeWert = 0
                          swInStream = new StringWriter();
                          ProcessHelper.startProcess( sCmd, null, fleExeDir, cancelable, iTimeout, swInStream, pwErr );

                          if( cancelable.isCanceled() )
                          {
                            pwLog.println( TubigConst.MESS_BERECHNUNG_ABGEBROCHEN );
                          }

                          // TODO Monika Ende-Token **ende** noch
                          // weiterverabeiten
                          bExeEnde = TubigCopyUtils.copyAndAnalyzeStreams( swInStream, pwLog, pwErr, cancelable );

                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
        sZeile = lneNumRdrBatch.readLine();
        if( cancelable.isCanceled() )
        {
          pwLog.println( TubigConst.MESS_BERECHNUNG_ABGEBROCHEN );
          return;
        }
      }
      pwLog.println( TubigConst.MESS_BERECHNUNG_BEENDET );
    }
    catch( final IOException e )
    {
      pwErr.println( "Fehlergrund (IOException): " + e.getCause() );
      pwErr.println( "Fehlermeldung: " + e.getLocalizedMessage() );
      e.printStackTrace();
      throw new TubigBatchException( cancelable, TubigBatchException.STATUS_ERROR, TubigConst.FINISH_ERROR_TEXT );
    }
    catch( final ProcessTimeoutException e )
    {
      pwErr.println( "Fehlergrund (ProcessTimeoutException): " + e.getCause() );
      pwErr.println( "Fehlermeldung: " + e.getLocalizedMessage() );
      e.printStackTrace();
      throw new TubigBatchException( cancelable, TubigBatchException.STATUS_ERROR, TubigConst.FINISH_ERROR_TEXT );
    }
    finally
    {
      pwErr.flush();
      pwLog.flush();
    }
  }

  public static void main( String[] args ) throws IOException, TubigBatchException
  {
    //    final File leseDatei = new File( System.getProperty( "java.io.tmpdir" ),
    // "RUN1.bat" );
    //    final File exeDir = new File( System.getProperty( "java.io.tmpdir" ) );
    final File leseDatei = new File( "C:/BODEVOR", "RUN1.bat" );
    //    final File logDatei = new File( "C:/BODEVOR", "bode_batch.log" );
    //    final File errDatei = new File( "C:/BODEVOR", "bode_batch.err" );
    final File exeDir = new File( "C:/BODEVOR" );
    final InputStreamReader reader = new FileReader( leseDatei );
    final PrintWriter wrtrErr = new PrintWriter( System.out );
    final PrintWriter wrtrLog = new PrintWriter( System.out );
    // ICancelable übergeben
    //   runBatch( exeDir, reader, wrtrLog, wrtrErr, null );
    //runBatch( exeDir, leseDatei, logDatei, errDatei, null );
    runBatch( exeDir, leseDatei, null );
    IOUtils.closeQuietly( reader );
    IOUtils.closeQuietly( wrtrLog );
    IOUtils.closeQuietly( wrtrErr );

  }
}