package org.kalypso.lhwsachsenanhalt.tubig;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypso.services.calculation.job.ICalcResultEater;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * <p>
 * Der Rechenservice für die Tubig-Modelle
 * </p>
 * 
 * @author Thül
 */
public class TubigCalcJob implements ICalcJob
{
  /**
   * @throws CalcJobServiceException
   * @see org.kalypso.services.calculation.job.ICalcJob#run(java.io.File,
   *      org.kalypso.services.calculation.job.ICalcDataProvider,
   *      org.kalypso.services.calculation.job.ICalcResultEater,
   *      org.kalypso.services.calculation.job.ICalcMonitor)
   */
  public void run( final File tmpdir, final ICalcDataProvider inputProvider,
      final ICalcResultEater resultEater, final ICalcMonitor monitor )
      throws CalcJobServiceException

  {
    final File outputDir; // Grundverzeichnis für Ergebnisse (Logs, Zeitreihen)
    final File ergDir; // Verzeichnis, in dem Ergebnisse im ZML-Format stehen
    // (Unterverzeichnis zu outputDir)
    final File logDir; // Verzeichnis, in dem Log-Dateien erzeugt werden
    // (Unterverzeichnis zu outputDir)
    final File logCopyErgDir; // Verzeichnis, in dem Logs zu den bceCpyErg_xy -
    // Batches stehen
    final File logCopyInDir; // Verzeichnis, in dem Logs zu den bceCpyIn_xy -
    // Batches stehen
    final File bodevorDir; // Verzeichnis, in dem gerechnet wird (BODEVOR bei
    // WinPro, calc hier auf Server)
    File dirHelp; // Hilfsverzeichnis

    final File fleCalcLog;
    final FileOutputStream strmLog;
    TubigCalculationData dataCrtl;

    PrintWriter pwCalcLog;
    String sBatNme;
    File fleBat;
    File fleBatLog;
    File fleBatErr;
    int ii;
    int iCntBat;

    pwCalcLog = null;
    dataCrtl = null;

    outputDir = new File( tmpdir, ICalcServiceConstants.OUTPUT_DIR_NAME );
    bodevorDir = new File( tmpdir, ICalcServiceConstants.CALC_DIR_NAME );
    ergDir = new File( outputDir, TubigConst.ERGEBNISSE );
    logDir = new File( outputDir, TubigConst.LOGS );
    logCopyErgDir = new File( outputDir, TubigConst.LOGS_COPYERG );
    logCopyInDir = new File( outputDir, TubigConst.LOGS_COPYIN );

    outputDir.mkdirs();
    bodevorDir.mkdirs();
    logDir.mkdirs();
    logCopyErgDir.mkdirs();
    logCopyInDir.mkdirs();

    fleCalcLog = new File( logDir, TubigConst.NAME_CALC_LOG );

    final Map metaMap = new HashMap();
    try
    {
      strmLog = new FileOutputStream( fleCalcLog );
      pwCalcLog = new PrintWriter( new BufferedWriter( new OutputStreamWriter( strmLog,
          TubigConst.TUBIG_CODEPAGE ) ) );

      // Eingabedateien erzeugen
      monitor.setMessage( TubigConst.MESS_DATEIEN_ERZEUGEN );
      pwCalcLog.println( TubigConst.MESS_DATEIEN_ERZEUGEN + " (" + TubigUtils.getAktuelleUhrzeit()
          + ")" );

      dataCrtl = TubigInputWorker.createCalcInput( bodevorDir, inputProvider, metaMap );

      // Schleife über die Batches (und jeweils starten)
      monitor.setMessage( TubigConst.MESS_RECHENKERN_AUFRUFEN );
      pwCalcLog.println( TubigConst.MESS_RECHENKERN_AUFRUFEN + " ("
          + TubigUtils.getAktuelleUhrzeit() + ")" );
      iCntBat = dataCrtl.getBatches().length;
      for( ii = 0; ii < iCntBat; ii++ )
      {
        sBatNme = dataCrtl.getBatches()[ii];
        monitor.setMessage( TubigConst.MESS_BERECHNUNG_WIRD_GESTARTET + " (" + sBatNme + ")" );
        pwCalcLog.println( TubigConst.MESS_BERECHNUNG_WIRD_GESTARTET + " (" + sBatNme + ", "
            + TubigUtils.getAktuelleUhrzeit() + ")" );

        // Falls erforderlich, werden Eingangsdaten noch manipuliert (umbenannt)
        fleBatLog = new File( logCopyInDir, TubigConst.NAME_BAT + "_" + TubigConst.PRE_COPY_IN_BATCH
            + sBatNme + TubigConst.NAME_EXT_LOG );
        fleBatErr = new File( logCopyInDir, TubigConst.NAME_BAT + "_" + TubigConst.PRE_COPY_IN_BATCH
            + sBatNme + TubigConst.NAME_EXT_ERR );
        fleBat = new File( bodevorDir, TubigConst.PRE_COPY_IN_BATCH + sBatNme );
        TubigBatchInterpreter.runBatch( bodevorDir, fleBat, fleBatLog, fleBatErr, monitor );
        if( monitor.isCanceled() )
        {
          pwCalcLog.println( TubigConst.MESS_BERECHNUNG_ABGEBROCHEN + " ("
              + TubigUtils.getAktuelleUhrzeit() + ")" );
          return;
        }

        // Ausführen der eigentlichen Batch-Datei
        fleBatLog = new File( logDir, TubigConst.NAME_BAT + "_" + sBatNme + TubigConst.NAME_EXT_LOG );
        fleBatErr = new File( logDir, TubigConst.NAME_BAT + "_" + sBatNme + TubigConst.NAME_EXT_ERR );
        fleBat = new File( bodevorDir, sBatNme );
        TubigBatchInterpreter.runBatch( bodevorDir, fleBat, fleBatLog, fleBatErr, monitor );
        if( monitor.isCanceled() )
        {
          pwCalcLog.println( TubigConst.MESS_BERECHNUNG_ABGEBROCHEN + " ("
              + TubigUtils.getAktuelleUhrzeit() + ")" );
          return;
        }

        // Ergebnis-Dateien werden in Unterverzeichnisse Pegel, bzw. Speicher
        // kopiert
        fleBatLog = new File( logCopyErgDir, TubigConst.NAME_BAT + "_" + TubigConst.PRE_COPY_OUT_BATCH
            + sBatNme + TubigConst.NAME_EXT_LOG );
        fleBatErr = new File( logCopyErgDir, TubigConst.NAME_BAT + "_" + TubigConst.PRE_COPY_OUT_BATCH
            + sBatNme + TubigConst.NAME_EXT_ERR );
        fleBat = new File( bodevorDir, TubigConst.PRE_COPY_OUT_BATCH + sBatNme );
        TubigBatchInterpreter.runBatch( bodevorDir, fleBat, fleBatLog, fleBatErr, monitor );
        if( monitor.isCanceled() )
        {
          pwCalcLog.println( TubigConst.MESS_BERECHNUNG_ABGEBROCHEN + " ("
              + TubigUtils.getAktuelleUhrzeit() + ")" );
          return;
        }

        monitor.setProgress( 33 );
        if( monitor.isCanceled() )
        {
          pwCalcLog.println( TubigConst.MESS_BERECHNUNG_ABGEBROCHEN + " ("
              + TubigUtils.getAktuelleUhrzeit() + ")" );
          return;
        }

        monitor.setProgress( 33 );
        if( monitor.isCanceled() )
        {
          pwCalcLog.println( TubigConst.MESS_BERECHNUNG_ABGEBROCHEN + " ("
              + TubigUtils.getAktuelleUhrzeit() + ")" );
          return;
        }

        monitor.setProgress( 34 );
        if( monitor.isCanceled() )
        {
          pwCalcLog.println( TubigConst.MESS_BERECHNUNG_ABGEBROCHEN + " ("
              + TubigUtils.getAktuelleUhrzeit() + ")" );
          return;
        }

        monitor.setMessage( TubigConst.MESS_BERECHNUNG_BEENDET + " (" + sBatNme + ")" );
        pwCalcLog.println( TubigConst.MESS_BERECHNUNG_BEENDET + " (" + sBatNme + ", "
            + TubigUtils.getAktuelleUhrzeit() + ")" );

      }
      monitor.setMessage( TubigConst.MESS_BERECHNUNG_BEENDET );
      pwCalcLog.println( TubigConst.MESS_BERECHNUNG_BEENDET + " ("
          + TubigUtils.getAktuelleUhrzeit() + ")" );

      monitor.setMessage( TubigConst.MESS_ERGEBNISSE_ZURUECK );
      pwCalcLog.println( TubigConst.MESS_ERGEBNISSE_ZURUECK + " ("
          + TubigUtils.getAktuelleUhrzeit() + ")" );
      // Besucher, der aus jeder TUBIG-Datei, die er in den
      // Unterverzeichnissen von CALC (Pegel und Speicher) findet, eine ZML
      // macht
      // und sie im ergDir in entsprechendes Verzeichnis schreibt.
      // ZML-Metadaten werden aus Input-Daten übernommen
      dirHelp = new File( bodevorDir, TubigConst.PEGEL );
      if( dirHelp.isDirectory() )
      {
        TubigFileVisitorTubig2Zml.writeZml( dirHelp, ergDir, TubigConst.PEGEL, inputProvider
            .getURLForID( "MODELL_GML" ), metaMap );
      }
      dirHelp = new File( bodevorDir, TubigConst.SPEICHER );
      if( dirHelp.isDirectory() )
      {
        TubigFileVisitorTubig2Zml.writeZml( dirHelp, ergDir, TubigConst.SPEICHER, inputProvider
            .getURLForID( "MODELL_GML" ), metaMap );
      }
      // IDs für addResult sind in der ModelSpec.xml (Client) und
      // tubigcalcjob_spec.xml (Server) festgelegt
      // Wenn Fehler beim Erzeugen der ZML-Dateien, dann sollen keine Ergebnisse
      // übertragen werden (Kuddelmuddel-Gefahr zu hoch)
      resultEater.addResult( TubigConst.ERGEBNISSE, ergDir );
    }
    catch( final FileNotFoundException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "Datei " + fleCalcLog.getName() + " nicht gefunden", e );
    }
    catch( final UnsupportedEncodingException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( "Encoding " + TubigConst.TUBIG_CODEPAGE
          + " wird für Calc-Log nicht unterstützt.", e );
    }
    catch( final TubigBatchException e )
    {
      // Fehler bei Abarbeitung einer Batch. Batches werden nicht weiter
      // abgearbeitet: kontrollierter Abbruch
      e.printStackTrace();
      pwCalcLog
          .println( "Bei der Abarbeitung einer Batch-Datei ist ein Fehler aufgetreten. Es werden keine Ergebnisse übertragen. Weitere Informationen finden sich in den Log-Dateien." );
    }
    catch( final TubigException e )
    {
      e.printStackTrace();
      throw new CalcJobServiceException( e.getLocalizedMessage(), e );
    }
    finally
    {
      IOUtils.closeQuietly( pwCalcLog );
      resultEater.addResult( TubigConst.LOGS, logDir );
    }
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getSpezifikation()
   */
  public URL getSpezifikation()
  {
    return getClass().getResource( TubigConst.CALCJOB_SPEC );
  }
}