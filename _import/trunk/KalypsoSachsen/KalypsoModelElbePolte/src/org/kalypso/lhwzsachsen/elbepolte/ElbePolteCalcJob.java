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
package org.kalypso.lhwzsachsen.elbepolte;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.io.FileCopyVisitor;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.commons.java.lang.ProcessHelper.ProcessTimeoutException;
import org.kalypso.lhwzsachsen.elbepolte.visitors.FileVisitorHwvs2Zml;
import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypso.services.calculation.job.ICalcResultEater;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * 
 * <p>
 * CalcJob for river Elbe (modelling software by IB Polte)
 * </p>
 * 
 * @author thuel2
 */
public class ElbePolteCalcJob implements ICalcJob
{

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider, org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( File tmpdir, ICalcDataProvider inputProvider, ICalcResultEater resultEater, ICalcMonitor monitor )
      throws CalcJobServiceException
  {
    final File outputDir = new File( tmpdir, ICalcServiceConstants.OUTPUT_DIR_NAME );
    outputDir.mkdirs();

    final File logfile = new File( outputDir, "elbePolte.log" );

    PrintWriter pw = null;
    FileWriter fw = null;
    final Map metaMap = new HashMap();
    try
    {
      fw = new FileWriter( logfile );
      pw = new PrintWriter( fw );

      pw.println( "Modell Berechnung wird gestartet (" + ElbePolteUtils.getAktuelleUhrzeit() + ")" );
      pw.println();

      if( monitor.isCanceled() )
      {
        monitor.setFinishInfo( ICalcServiceConstants.FINISHED, ElbePolteConst.CALC_CANCELLED );
        pw.println( ElbePolteConst.CALC_CANCELLED );
        return;
      }
      final Properties props = new Properties();
      monitor.setMessage( "Dateien für Rechenkern werden erzeugt" );
      pw.println( "Dateien für Rechenkern werden erzeugt" );

      final File nativedir = new File( tmpdir, ".native" );
      final File nativeInDir = new File( nativedir, "in" );
      final File nativeOutDir = new File( nativedir, "out" );
      nativedir.mkdirs();
      nativeInDir.mkdirs();
      nativeOutDir.mkdirs();

      final File exeDir = ElbePolteInputWorker.createNativeInput( tmpdir, inputProvider, pw, props, nativeInDir,
          metaMap );

      resultEater.addResult( "NATIVE_IN_DIR", nativeInDir );

      monitor.setProgress( 33 );
      if( monitor.isCanceled() )
      {
        monitor.setFinishInfo( ICalcServiceConstants.FINISHED, ElbePolteConst.CALC_CANCELLED );
        pw.println( ElbePolteConst.CALC_CANCELLED );
        return;
      }

      monitor.setMessage( ElbePolteConst.CALC_CALL );
      pw.println( ElbePolteConst.CALC_CALL );
      startCalculation( exeDir, pw, monitor, nativeOutDir );

      resultEater.addResult( "NATIVE_OUT_DIR", nativeOutDir );

      monitor.setProgress( 33 );
      if( monitor.isCanceled() )
      {
        monitor.setFinishInfo( ICalcServiceConstants.FINISHED, ElbePolteConst.CALC_CANCELLED );
        pw.println( ElbePolteConst.CALC_CANCELLED );
        return;
      }

      monitor.setMessage( "Ergebnisse werden zurückgelesen" );
      pw.println( "Ergebnisse werden zurückgelesen" );
      try
      {
        writeResultsToFolder( nativeOutDir, outputDir, props, metaMap );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        throw new CalcJobServiceException( "Fehler beim Schreiben der Ergebnis-Zeitreihen", e );
      }

      monitor.setProgress( 34 );
      if( monitor.isCanceled() )
      {
        monitor.setFinishInfo( ICalcServiceConstants.FINISHED, ElbePolteConst.CALC_CANCELLED );
        pw.println( ElbePolteConst.CALC_CANCELLED );
        return;
      }

      monitor.setMessage( ElbePolteConst.CALC_FINISHED );
      pw.println( ElbePolteConst.CALC_FINISHED );

    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new CalcJobServiceException( "Fehler bei der Berechnung:\n" + e.getLocalizedMessage(), e );
    }
    finally
    {
      IOUtils.closeQuietly( pw );
      IOUtils.closeQuietly( fw );
      resultEater.addResult( "LOG", logfile );
      resultEater.addResult( "ERGEBNISSE", new File( outputDir, ICalcServiceConstants.RESULT_DIR_NAME ) );
      monitor.setFinishInfo( ICalcServiceConstants.FINISHED, ElbePolteConst.CALC_FINISHED );
    }
  }

  /**
   * @param nativeOutDir
   * @param outputDir
   * @param props
   * @throws IOException
   */

  private void writeResultsToFolder( File nativeOutDir, File outputDir, Properties props, Map metaMap )
      throws Exception
  {

    final FileVisitorHwvs2Zml fleVisitorPegel = new FileVisitorHwvs2Zml( outputDir, props,
        ElbePolteConst.GML_ELBE_PEGEL_COLL, "ganglinie_modellwerte", true, metaMap );
    FileUtilities.accept( nativeOutDir, fleVisitorPegel, true );

    final FileVisitorHwvs2Zml fleVisitorZwge = new FileVisitorHwvs2Zml( outputDir, props,
        ElbePolteConst.GML_H_PEGEL_COLL, "ganglinie_modellwerte", true, metaMap );
    FileUtilities.accept( nativeOutDir, fleVisitorZwge, true );

    if( fleVisitorPegel.hasException() )
      throw new Exception( "Fehler beim Schreiben der Pegel-Zeitreihen ins ZML-Format: "
          + fleVisitorPegel.getExceptions()[0].getLocalizedMessage(), fleVisitorPegel.getExceptions()[0] );
    if( fleVisitorZwge.hasException() )
      throw new Exception( "Fehler beim Schreiben der Zwischengebiets-Zeitreihen ins ZML-Format: "
          + fleVisitorZwge.getExceptions()[0].getLocalizedMessage(), fleVisitorZwge.getExceptions()[0] );
  }

  /**
   * @param exeDir
   * @param pw
   * @param monitor
   * @param nativeOutDir
   */
  private void startCalculation( File exeDir, PrintWriter pw, ICalcMonitor monitor, File nativeOutDir )
      throws CalcJobServiceException
  {
    final String commandString = exeDir + File.separator + "HWObereElbe.exe";

    pw.println( commandString );

    try
    {
      // timeout after 10 sec
      ProcessHelper.startProcess( commandString, null, exeDir, monitor, 10000, null, null );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "Fehler beim Ausführen der HWObereElbe.exe", e );
    }
    catch( final ProcessTimeoutException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "Fehler beim Ausführen der HWObereElbe.exe", e );
    }
    finally
    {
      // fehler.txt lesen und analysieren
      File exeModellDir = new File( exeDir, "Modell" );
      final File fleFehler = new File( exeModellDir, "Fehler.txt" );
      try
      {
        final InputStreamReader isr = new InputStreamReader( new FileInputStream( fleFehler ) );
        final BufferedReader reader = new BufferedReader( isr );

        pw.println( "Ausgaben des Rechenkerns" );
        pw.println( "========================" );
        pw.println( "=   Standard-Ausgabe   =" );
        pw.println( "========================" );
        final LineNumberReader lneNumRead = new LineNumberReader( reader );
        String processOut = lneNumRead.readLine();
        String processOut2 = "";

        while( processOut != null )
        {
          pw.println( processOut );
          processOut2 = processOut;
          processOut = lneNumRead.readLine();
        }
        pw.println( "========================" );
        pw.println();

        if( processOut2.startsWith( "Keine Fehler" ) )
        {
          pw.println( "Rechnung erfolgreich beendet." );
          final File nativeOutModellDir = new File( nativeOutDir, "Modell" );
          if( nativeOutDir.exists() && exeModellDir.exists() )
          {
            final FileCopyVisitor copyVisitor = new FileCopyVisitor( exeModellDir, nativeOutModellDir, true );
            FileUtilities.accept( exeModellDir, copyVisitor, true );
          }
        }
        else
        {
          pw.println( "Rechnung nicht erfolgreich beendet." );
          throw new Exception( processOut2, new Exception() );
        }
      }
      catch( Exception e )
      {
        throw new CalcJobServiceException( e.getLocalizedMessage(), e );
      }
    }
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation()
  {
    return getClass().getResource( ElbePolteConst.CALCJOB_SPEC );
  }
}
