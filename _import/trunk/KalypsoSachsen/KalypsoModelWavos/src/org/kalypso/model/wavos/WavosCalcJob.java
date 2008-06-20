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

import java.io.File;
import java.io.FileOutputStream;
import java.net.URL;
import java.util.logging.Logger;
import java.util.logging.StreamHandler;

import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypso.services.calculation.job.ICalcResultEater;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author thuel2
 */
public class WavosCalcJob implements ICalcJob
{
  private final Logger m_logger = Logger.getLogger( getClass().getName() );

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider, org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( File tmpdir, ICalcDataProvider inputProvider, ICalcResultEater resultEater, ICalcMonitor monitor )
      throws CalcJobServiceException
  {
    //    modelSpec
    //    <input id="GML" description="Modelldaten"/>
    //    <input id="CONTROL_GML" description="Steuerdaten"/>
    //    
    //    <input id="ZML_PEGEL" description="Pegel-Zeitreihen"/>
    //    <input id="ZML_ZUFLUESSE" description="Zufluss-Zeitreihen"/>
    //    <input id="ZML_POLDER" description="Polder-Zeitreihen"/>
    //    
    //
    //    <!-- Ergebnis-Dateien/Verzeichnisse -->
    //    <output id="LOG" description="Log-Dateien"/>
    //    <output id="ERGEBNISSE" description="Ergebniszeitreihen"/>
    //    <output id="NATIVE_IN_DIR" description="Eingabedaten (natives Format)"/>
    //    <output id="NATIVE_OUT_DIR" description="Ausgabedaten (natives Format)"/>
 
    // Logger wie bei der Saale
    // Eingabedateien erzeugen
    // Zeitreihen
    // AT-Dateien
    // Steuerdateien?

    // dr_wavos starten

    // LOG-Dateien
    // Ergebnisse holen

    //    ElbePolte
    //    final File outputDir = new File( tmpdir, ICalcServiceConstants.OUTPUT_DIR_NAME );
    //    outputDir.mkdirs();
    //
    //    final File logfile = new File( outputDir, "elbePolte.log" );
    //
    //    PrintWriter pw = null;
    //    FileWriter fw = null;
    //    final Map metaMap = new HashMap();
    //    try
    //    {

    //###SAALE-Logger
    final File loggerFile = new File( tmpdir, "elbeWavosSachsen.log" );
    resultEater.addResult( "LOG", loggerFile );
    StreamHandler streamHandler = null;
    try
    {
      final FileOutputStream loggerStream = new FileOutputStream( loggerFile );
      final Logger packageLogger = Logger.getLogger( getClass().getPackage().getName() );
      streamHandler = new StreamHandler( loggerStream, new SimplisticFormatter() );
      // TODO: besser als info vom client holen? oder andersrum, das encoding weitergeben an den client?
      streamHandler.setEncoding( WavosConst.WAVOS_CODEPAGE );
      packageLogger.addHandler( streamHandler );

    }
    //      fw = new FileWriter( logfile );
    //      pw = new PrintWriter( fw );
    //
    //      pw.println( "Modell Berechnung wird gestartet (" + ElbePolteUtils.getAktuelleUhrzeit() + ")" );
    //      pw.println();
    //
    //      if( monitor.isCanceled() )
    //      {
    //        monitor.setFinishInfo( ICalcServiceConstants.FINISHED, ElbePolteConst.CALC_CANCELLED );
    //        pw.println( ElbePolteConst.CALC_CANCELLED );
    //        return;
    //      }
    //      final Properties props = new Properties();
    //      monitor.setMessage( "Dateien für Rechenkern werden erzeugt" );
    //      pw.println( "Dateien für Rechenkern werden erzeugt" );
    //
    //      final File nativedir = new File( tmpdir, ".native" );
    //      final File nativeInDir = new File( nativedir, "in" );
    //      final File nativeOutDir = new File( nativedir, "out" );
    //      nativedir.mkdirs();
    //      nativeInDir.mkdirs();
    //      nativeOutDir.mkdirs();
    //
    //      final File exeDir = ElbePolteInputWorker.createNativeInput( tmpdir, inputProvider, pw, props, nativeInDir,
    //          metaMap );
    //
    //      resultEater.addResult( "NATIVE_IN_DIR", nativeInDir );
    //
    //      monitor.setProgress( 33 );
    //      if( monitor.isCanceled() )
    //      {
    //        monitor.setFinishInfo( ICalcServiceConstants.FINISHED, ElbePolteConst.CALC_CANCELLED );
    //        pw.println( ElbePolteConst.CALC_CANCELLED );
    //        return;
    //      }
    //
    //      monitor.setMessage( ElbePolteConst.CALC_CALL );
    //      pw.println( ElbePolteConst.CALC_CALL );
    //      startCalculation( exeDir, pw, monitor, nativeOutDir );
    //
    //      resultEater.addResult( "NATIVE_OUT_DIR", nativeOutDir );
    //
    //      monitor.setProgress( 33 );
    //      if( monitor.isCanceled() )
    //      {
    //        monitor.setFinishInfo( ICalcServiceConstants.FINISHED, ElbePolteConst.CALC_CANCELLED );
    //        pw.println( ElbePolteConst.CALC_CANCELLED );
    //        return;
    //      }
    //
    //      monitor.setMessage( "Ergebnisse werden zurückgelesen" );
    //      pw.println( "Ergebnisse werden zurückgelesen" );
    //      try
    //      {
    //        writeResultsToFolder( nativeOutDir, outputDir, props, metaMap );
    //      }
    //      catch( final Exception e )
    //      {
    //        e.printStackTrace();
    //        throw new CalcJobServiceException( "Fehler beim Schreiben der Ergebnis-Zeitreihen", e );
    //      }
    //
    //      monitor.setProgress( 34 );
    //      if( monitor.isCanceled() )
    //      {
    //        monitor.setFinishInfo( ICalcServiceConstants.FINISHED, ElbePolteConst.CALC_CANCELLED );
    //        pw.println( ElbePolteConst.CALC_CANCELLED );
    //        return;
    //      }
    //
    //      monitor.setMessage( ElbePolteConst.CALC_FINISHED );
    //      pw.println( ElbePolteConst.CALC_FINISHED );
    //
    //    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new CalcJobServiceException( "Fehler bei der Berechnung:\n" + e.getLocalizedMessage(), e );
    }
    finally
    {
      //      IOUtils.closeQuietly( pw );
      //      IOUtils.closeQuietly( fw );
      //      resultEater.addResult( "LOG", logfile );
      //      resultEater.addResult( "ERGEBNISSE", new File( outputDir, ICalcServiceConstants.RESULT_DIR_NAME ) );
      //      monitor.setFinishInfo( ICalcServiceConstants.FINISHED, ElbePolteConst.CALC_FINISHED );
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
