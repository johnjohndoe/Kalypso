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
 *  g.belger@bjoernsen.de
 *  m.schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wavos;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.StringWriter;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;
import java.util.logging.StreamHandler;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.io.FileCopyVisitor;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.commons.java.lang.ProcessHelper.ProcessTimeoutException;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationConstants;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

/**
 * 
 * @author thuel2
 */
public class WavosCalcJob implements ISimulation
{
  private final Logger m_logger = Logger.getLogger( getClass().getName() );

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider, org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( File tmpdir, ISimulationDataProvider inputProvider, ISimulationResultEater resultEater, ISimulationMonitor monitor )
      throws SimulationException
  {
    final File loggerFile = new File( tmpdir, WavosConst.LOG_FILE );
    resultEater.addResult( "LOG", loggerFile );
    StreamHandler streamHandler = null;
    final Map metaMap = new HashMap();
    try
    {
      final FileOutputStream loggerStream = new FileOutputStream( loggerFile );
      final Logger packageLogger = Logger.getLogger( getClass().getPackage().getName() );
      streamHandler = new StreamHandler( loggerStream, new SimplisticFormatter() );
      streamHandler.setEncoding( WavosConst.WAVOS_CODEPAGE );
      packageLogger.addHandler( streamHandler );

      m_logger.info( WavosConst.CALC_START + " (" + WavosUtils.getAktuelleUhrzeit() + ")" );
      if( monitor.isCanceled() )
      {
        monitor.setFinishInfo( IStatus.CANCEL, WavosConst.CALC_CANCELLED );
        m_logger.info( WavosConst.CALC_CANCELLED );
        return;
      }

      monitor.setMessage( WavosConst.CALC_FILE_CREATION );
      m_logger.info( WavosConst.CALC_FILE_CREATION );

      final File nativedir = new File( tmpdir, ".native" );
      final File nativeInDir = new File( nativedir, "in" );
      final File nativeOutDir = new File( nativedir, "out" );
      final File outputDir = new File( tmpdir, ISimulationConstants.OUTPUT_DIR_NAME );
      nativedir.mkdirs();
      nativeInDir.mkdirs();
      nativeOutDir.mkdirs();
      outputDir.mkdirs();

      resultEater.addResult( "NATIVE_IN_DIR", nativeInDir );
      resultEater.addResult( "NATIVE_OUT_DIR", nativeOutDir );
      //      final File outputResultDir = new File( outputDir, ICalcServiceConstants.RESULT_DIR_NAME );
      //      resultEater.addResult( "ERGEBNISSE", outputResultDir );
      resultEater.addResult( "ERGEBNISSE", outputDir );

      // Eingabedateien erzeugen
      final Properties props = new Properties();
      final File exeDir = WavosInputWorker.createNativeInput( tmpdir, inputProvider, props, nativeInDir,
          WavosConst.FLUSS, metaMap );

      monitor.setProgress( 33 );
      if( monitor.isCanceled() )
      {
        monitor.setFinishInfo( IStatus.CANCEL, WavosConst.CALC_CANCELLED );
        m_logger.info( WavosConst.CALC_CANCELLED );
        return;
      }

      monitor.setMessage( WavosConst.CALC_CALL );
      m_logger.info( WavosConst.CALC_CALL );
      // dr_wavos starten
      startCalculation( exeDir, monitor, nativeOutDir );

      monitor.setProgress( 33 );
      if( monitor.isCanceled() )
      {
        monitor.setFinishInfo( IStatus.CANCEL, WavosConst.CALC_CANCELLED );
        m_logger.info( WavosConst.CALC_CANCELLED );
        return;
      }
      //
      monitor.setMessage( WavosConst.CALC_RESULT_READ );
      m_logger.info( WavosConst.CALC_RESULT_READ );
      // Ergebnisse holen
      try
      {
        writeResultsToFolder( nativeOutDir, outputDir, props, metaMap );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        throw new SimulationException( "Fehler beim Schreiben der Ergebnis-Zeitreihen", e );
      }

      monitor.setProgress( 34 );
      if( monitor.isCanceled() )
      {
        monitor.setFinishInfo( IStatus.CANCEL, WavosConst.CALC_CANCELLED );
        m_logger.info( WavosConst.CALC_CANCELLED );
        return;
      }

      monitor.setMessage( WavosConst.CALC_FINISHED );
      m_logger.info( WavosConst.CALC_FINISHED );

    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new SimulationException( "Fehler bei der Berechnung:\n" + e.getLocalizedMessage(), e );
    }
    finally
    {
      monitor.setFinishInfo( IStatus.OK, WavosConst.CALC_FINISHED );
      if( streamHandler != null )
        streamHandler.close();
    }

  }

  private void writeResultsToFolder( final File nativeOutDir, final File outputDir, final Properties props,
      final Map metaMap ) throws Exception
  {
    final File nativeOutWavosDir = new File( nativeOutDir, WavosConst.DIR_WAVOS );
    final File nativeOutWavosFlussDir = new File( nativeOutWavosDir, WavosConst.FLUSS );
    final File nativeOutAwerteDir = new File( nativeOutWavosFlussDir, WavosConst.DIR_AWERTE );
    // Anfangswerte zippen (zip to new destiny)
    final File outputAwerteDir = new File( outputDir, WavosConst.DIR_ANFANGSWERTE );
    outputAwerteDir.mkdirs();
    final File outputAwerteFile = new File( outputAwerteDir, WavosConst.FILE_AWERTE_ZIP );
    ZipUtilities.zip( outputAwerteFile, nativeOutAwerteDir );

    // Zeitreihen zurücklesen
    // --- "normale" Ergebnisse
    final File outputTSDir = new File( outputDir, WavosConst.DIR_ZEITREIHEN );
    outputTSDir.mkdirs();
    final File outputZmlDir = new File( outputTSDir, "Pegel" );
    outputZmlDir.mkdirs();
    final File nativeOutVorherDir = new File( new File( new File( nativeOutDir, WavosConst.DIR_WAVOS ),
        WavosConst.FLUSS ), WavosConst.DIR_VORHER );
    WavosConverter.convertVorher2Zml( nativeOutVorherDir, outputZmlDir, props, metaMap, true );

    // --- ohne Anbiegung (Ergebnis wird vor shiftvor zwischengespeichert)
    final File outputTSOhneShiftvorDir = new File( outputDir, WavosConst.DIR_ZEITREIHEN_OHNE_SHIFTVOR );
    outputTSOhneShiftvorDir.mkdirs();
    final File outputZmlOhneShiftvorDir = new File( outputTSOhneShiftvorDir, "Pegel" );
    outputZmlOhneShiftvorDir.mkdirs();

        final File nativeOutVorherSaveDir = new File( new File( new File( nativeOutDir, WavosConst.DIR_WAVOS ),
            WavosConst.FLUSS ), WavosConst.DIR_VORHER_SAVE );
        WavosConverter.convertVorher2Zml( nativeOutVorherSaveDir, outputZmlOhneShiftvorDir, props, metaMap, false );
  }

  private void startCalculation( final File exeDir, final ISimulationMonitor monitor, final File nativeOutDir )
      throws IOException, SimulationException
  {

    final String commandString = exeDir + File.separator + WavosConst.FILE_START_BAT;
    m_logger.info( "\t" + commandString );

    final StringWriter logStream = new StringWriter();
    final StringWriter errStream = new StringWriter();
    try
    {
      // timeout after 5 min
      ProcessHelper.startProcess( commandString, null, exeDir, monitor, 1000 * 60 * 5, logStream, errStream );

      m_logger.info( "Ausgaben des Rechenkerns" );
      m_logger.info( "========================" );
      m_logger.info( "= Standard-Ausgabe (Konsole) =" );
      m_logger.info( "========================" );
      m_logger.info( logStream.toString() );
      m_logger.info( "========================" );
      m_logger.info( "" );

      final String errString = errStream.toString();
      if( errString.length() > 0 )
      {
        m_logger.info( "========================" );
        m_logger.info( "= Fehler-Ausgabe (Konsole) =" );
        m_logger.info( "========================" );
        m_logger.info( errString );
        m_logger.info( "========================" );
        m_logger.info( "" );
      }
    }

    catch( final IOException e )
    {
      e.printStackTrace();
      throw new SimulationException( "Fehler beim Ausführen von WAVOS", e );
    }
    catch( final ProcessTimeoutException e )
    {
      e.printStackTrace();
      throw new SimulationException( "Fehler beim Ausführen von WAVOS", e );
    }
    finally
    {
      logStream.close();
      errStream.close();

      // LOG-Dateien (output.log, shiftvor.log) lesen und analysieren
      final File fleWavosLog = new File( exeDir, WavosConst.FILE_WAVOS_LOG );
      final File fleShiftvorLog = new File( exeDir, WavosConst.FILE_SHIFTVOR_LOG );
      boolean wavosSuccess = false;
      boolean shiftvorSuccess = false;
      InputStreamReader isrWavos = null;
      BufferedReader readerWavos = null;
      InputStreamReader isrShiftvor = null;
      BufferedReader readerShiftvor = null;
      try
      {
        isrWavos = new InputStreamReader( new FileInputStream( fleWavosLog ) );
        readerWavos = new BufferedReader( isrWavos );

        m_logger.info( "Ausgaben des Rechenkerns" );
        m_logger.info( "========================" );
        m_logger.info( "= LOG-Datei WAVOS (" + WavosConst.FILE_WAVOS_LOG + ") =" );
        m_logger.info( "========================" );
        final LineNumberReader lneNumRead = new LineNumberReader( readerWavos );
        String processOut = lneNumRead.readLine();
        String processOut2 = "";
        String processOut3 = "";

        while( processOut != null )
        {
          m_logger.info( processOut );
          processOut3 = processOut2;
          processOut2 = processOut;
          processOut = lneNumRead.readLine();
        }
        m_logger.info( "========================" );
        m_logger.info( "" );

        if( processOut2.startsWith( "Ende: call_rodasp: 1" ) )
        {
          m_logger.info( "Rechnung erfolgreich beendet." );
          wavosSuccess = true;
        }
        else if( processOut3.startsWith( "Ende: call_rodasp: -4" ) )
        {
          // für Fehler -4 spezielle Fehlermedlung von Frau Rademacher (BfG) bekommen
          m_logger.info( WavosConst.ERROR_MINUS_4 );
          throw new Exception( processOut3 + ": " + WavosConst.ERROR_MINUS_4, new Exception() );
        }
        else if( processOut3.startsWith( "Ende: call_rodasp: -" ) )
        {
          // call_rodasp <0 bedeutet Fehler
          m_logger.info( WavosConst.ERROR_MINUS );
          throw new Exception( processOut3 + ": " + WavosConst.ERROR_MINUS, new Exception() );
        }
        else
        {
          // call_rodasp =0 oder >1 sollten eigentlich nicht auftreten
          m_logger.info( WavosConst.ERROR_PLUS );
          throw new Exception( processOut3 + ": " + WavosConst.ERROR_PLUS, new Exception() );
        }

        // Shiftvor
        isrShiftvor = new InputStreamReader( new FileInputStream( fleShiftvorLog ) );
        readerShiftvor = new BufferedReader( isrShiftvor );

        m_logger.info( "Ausgaben des Rechenkerns" );
        m_logger.info( "========================" );
        m_logger.info( "= LOG-Datei SHIFTVOR (" + WavosConst.FILE_SHIFTVOR_LOG + ") =" );
        m_logger.info( "========================" );
        final LineNumberReader lneNumReadShift = new LineNumberReader( readerShiftvor );
        processOut = lneNumReadShift.readLine();
        processOut2 = "";
        processOut3 = "";

        while( processOut != null )
        {
          m_logger.info( processOut );
          processOut3 = processOut2;
          processOut2 = processOut;
          processOut = lneNumReadShift.readLine();
        }
        m_logger.info( "========================" );
        m_logger.info( "" );

        // TODO return value von shiftvor noch wirklich analysieren
        //        if( processOut2.startsWith( "" ) )
        //        {
        m_logger.info( "Shiftvor erfolgreich beendet." );
        shiftvorSuccess = true;
        //        }
        //        else
        //        {
        //          m_logger.info( WavosConst.ERROR_SHIFTVOR );
        //          throw new Exception( processOut2 + ": " + WavosConst.ERROR_SHIFTVOR, new Exception() );
        //        }
        if( wavosSuccess && shiftvorSuccess && nativeOutDir.exists() )
        {
          //  Daten zurückholen ins .native/out

          // vorher
          final File flussExeDir = new File( exeDir, WavosConst.FLUSS );
          final File nativeOutWavosDir = new File( nativeOutDir, WavosConst.DIR_WAVOS );
          final File nativeOutWavosFlussDir = new File( nativeOutWavosDir, WavosConst.FLUSS );
          final File nativeOutVorherDir = new File( nativeOutWavosFlussDir, WavosConst.DIR_VORHER );
          nativeOutVorherDir.mkdirs();
          final File exeVorherDir = new File( flussExeDir, WavosConst.DIR_VORHER );
          if( nativeOutVorherDir.exists() && exeVorherDir.exists() )
          {
            final FileCopyVisitor copyVisitor = new FileCopyVisitor( exeVorherDir, nativeOutVorherDir, true );
            FileUtilities.accept( exeVorherDir, copyVisitor, true );
          }
          
          // vorher_save
          final File nativeOutVorherSaveDir = new File( nativeOutWavosFlussDir, WavosConst.DIR_VORHER_SAVE );
          nativeOutVorherSaveDir.mkdirs();
          final File exeVorherSaveDir = new File( flussExeDir, WavosConst.DIR_VORHER_SAVE );
          if( nativeOutVorherSaveDir.exists() && exeVorherSaveDir.exists() )
          {
            final FileCopyVisitor copyVisitor = new FileCopyVisitor( exeVorherSaveDir, nativeOutVorherSaveDir, true );
            FileUtilities.accept( exeVorherSaveDir, copyVisitor, true );
          }
          // bin (vor allem wegen der SIM-Dateien)
          final File nativeOutBinDir = new File( nativeOutWavosFlussDir, WavosConst.DIR_BIN );
          nativeOutBinDir.mkdirs();
          final File exeBinDir = new File( flussExeDir, WavosConst.DIR_BIN );
          if( nativeOutBinDir.exists() && exeBinDir.exists() )
          {
            final FileCopyVisitor copyVisitor = new FileCopyVisitor( exeBinDir, nativeOutBinDir, true );
            FileUtilities.accept( exeBinDir, copyVisitor, true );
          }
          // awerte (ggf. nicht alle zurückübertragen...)
          final File nativeOutAwerteDir = new File( nativeOutWavosFlussDir, WavosConst.DIR_AWERTE );
          nativeOutAwerteDir.mkdirs();
          final File exeAwerteDir = new File( flussExeDir, WavosConst.DIR_AWERTE );
          if( nativeOutAwerteDir.exists() && exeAwerteDir.exists() )
          {
            final FileCopyVisitor copyVisitor = new FileCopyVisitor( exeAwerteDir, nativeOutAwerteDir, true );
            FileUtilities.accept( exeAwerteDir, copyVisitor, true );
          }

          // logs (output.log, shiftvor.log)
          FileUtils.copyFile( fleWavosLog, new File( nativeOutWavosFlussDir, WavosConst.FILE_WAVOS_LOG ) );
          FileUtils.copyFile( fleShiftvorLog, new File( nativeOutWavosFlussDir, WavosConst.FILE_SHIFTVOR_LOG ) );
        }

      }
      catch( Exception e )
      {
        throw new SimulationException( e.getLocalizedMessage(), e );
      }
      finally
      {
        IOUtils.closeQuietly( readerWavos );
        IOUtils.closeQuietly( isrWavos );
        IOUtils.closeQuietly( readerShiftvor );
        IOUtils.closeQuietly( isrShiftvor );
      }
    }
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation()
  {
    return getClass().getResource( WavosConst.CALCJOB_SPEC );
  }

}
