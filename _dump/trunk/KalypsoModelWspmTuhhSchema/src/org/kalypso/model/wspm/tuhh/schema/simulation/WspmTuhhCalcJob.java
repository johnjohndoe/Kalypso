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
package org.kalypso.model.wspm.tuhh.schema.simulation;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileFilter;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.net.URL;

import org.apache.commons.io.IOUtils;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation.ExeVersion;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation.MODE;
import org.kalypso.model.wspm.tuhh.core.wspwin.WspWinExporter;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.util.LogHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeaturePath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;

/**
 * @author Monika Thül
 * @author Gernot Belger
 */
public class WspmTuhhCalcJob implements ISimulation
{
  public static final String CALCJOB_SPEC = "WspmTuhhCalcJob_spec.xml";

  public static final String INPUT_OVW_MAP_SPECIAL = "OVW_MAP_SPECIAL";

  public static final String INPUT_OVW_MAP_GENERAL = "OVW_MAP_GENERAL";

  public static final String INPUT_EPS_THINNING = "EPS_THINNING";

  public static final String INPUT_CALC_PATH = "CALC_PATH";

  public static final String INPUT_MODELL_GML = "MODELL_GML";

  public static final String OUTPUT_QINTERVALL_RESULT_GMV = "qIntervallResultGmv";

  public static final String OUTPUT_QINTERVALL_RESULT = "qIntervallResultGml";

  private final PrintStream m_calcOutConsumer;

  public WspmTuhhCalcJob( )
  {
    this( System.out );
  }

  public WspmTuhhCalcJob( final PrintStream calcOutConsumer )
  {
    m_calcOutConsumer = calcOutConsumer;
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#run(java.io.File, org.kalypso.simulation.core.ISimulationDataProvider,
   *      org.kalypso.simulation.core.ISimulationResultEater, org.kalypso.simulation.core.ISimulationMonitor)
   */
  public void run( final File tmpDir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final URL modellGmlURL = (URL) inputProvider.getInputForID( INPUT_MODELL_GML );
    final String calcXPath = (String) inputProvider.getInputForID( INPUT_CALC_PATH );
    final String epsThinning = (String) inputProvider.getInputForID( INPUT_EPS_THINNING );
    URL ovwMapURL = null;
    if( inputProvider.hasID( INPUT_OVW_MAP_GENERAL ) )
      ovwMapURL = (URL) inputProvider.getInputForID( INPUT_OVW_MAP_GENERAL );
    if( inputProvider.hasID( INPUT_OVW_MAP_SPECIAL ) )
      ovwMapURL = (URL) inputProvider.getInputForID( INPUT_OVW_MAP_SPECIAL );

    final File simulogFile = new File( tmpDir, "simulation.log" );
    resultEater.addResult( "SimulationLog", simulogFile );

    PrintWriter pwSimuLog = null;
    InputStream zipInputStream = null;
    OutputStream strmKernelErr = null;
    InputStream mapContentStream = null;
    PrintWriter mapWriter = null;
    try
    {
      final PrintStream calcOutConsumer = m_calcOutConsumer;
      final OutputStream osSimuLog = new BufferedOutputStream( new FileOutputStream( simulogFile ) )
      {
        // REMARK: also stream stuff into System.out in order to have a log in the console.view
        /**
         * @see java.io.BufferedOutputStream#write(byte[], int, int)
         */
        @Override
        public synchronized void write( byte[] b, int off, int len ) throws IOException
        {
          super.write( b, off, len );

          calcOutConsumer.write( b, off, len );
        }

        /**
         * @see java.io.BufferedOutputStream#write(int)
         */
        @Override
        public synchronized void write( int b ) throws IOException
        {
          super.write( b );

          calcOutConsumer.write( b );
        }
      };
      pwSimuLog = new PrintWriter( new OutputStreamWriter( osSimuLog, "CP1252" ) );

      final LogHelper log = new LogHelper( pwSimuLog, monitor, calcOutConsumer );
      log.log( true, "Eingangsdaten werden geladen...", calcXPath );

      final GMLXPath calcpath = new GMLXPath( calcXPath, null );

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( modellGmlURL, null );

      final Object calcObject = GMLXPathUtilities.query( calcpath, workspace );
      if( !(calcObject instanceof Feature) )
      {
        monitor.setFinishInfo( IStatus.ERROR, "GMLXPath zeigt auf kein Feature: " + calcObject );
        log.log( false, "GMLXPath (%s) zeigt auf kein Feature: %s", calcXPath, calcObject );
        return;
      }

      final Feature calculationFeature = (Feature) calcObject;
      final TuhhCalculation calculation = new TuhhCalculation( calculationFeature );

      monitor.setProgress( 10 );

      log.log( true, "Schreibe Dateien für Kalypso-1D..." );

      // write calculation to tmpDir
      WspWinExporter.writeForTuhhKernel( calculation, tmpDir );

      // ensure availability of DATH directory (for results)
      final File dathDir = new File( tmpDir, "dath" );
      dathDir.mkdirs();

      // unpack kernel into tmpDir
      zipInputStream = new BufferedInputStream( WspmTuhhCalcJob.class.getResourceAsStream( "resources/rechenkern.zip" ) );
      ZipUtilities.unzip( zipInputStream, tmpDir, false );
      zipInputStream.close();

      // prepare kernel logs (log and err)
      final File fleKernelErr = new File( tmpDir, "kernel.err" );
      resultEater.addResult( "KernelErr", fleKernelErr );
      strmKernelErr = new BufferedOutputStream( new FileOutputStream( fleKernelErr ) );

      final File iniFile = new File( tmpDir, "kalypso-1D.ini" );

      monitor.setProgress( 20 );

      /* Start kalypso-1d.exe */
      log.log( true, "Rechenkern 'Kalypso-1D.exe' wird gestartet..." );

      if( log.checkCanceled() )
        return;

      final ExeVersion version = calculation.getVersion();
      if( version == null )
        throw new SimulationException( "Version des Rechenkerns nicht angegeben. Die Version muss in den Steuerparametern gesetzt werden.", null );

      // start calculation; the out-stream gets copied into the simulation.log and the system.out
      final File exeFile = new File( tmpDir, "Kalypso-1D" + version.name() + ".exe" );
      final String sCmd = "\"" + exeFile.getAbsolutePath() + "\" n \"" + iniFile.getAbsolutePath() + "\"";
      ProcessHelper.startProcess( sCmd, null, tmpDir, monitor, 0, osSimuLog, strmKernelErr, null );

      if( log.checkCanceled() )
        return;

      // load results + copy to result folder + unzip templates
      monitor.setProgress( 80 );
      monitor.setMessage( "Lade Ergebnisse" );
      pwSimuLog.println( "Ergebnis-Dateien werden generiert und übertragen." );

      // alle Modi
      final File ctrlFile = new File( dathDir, "Kontroll.log" );
      if( ctrlFile.exists() )
      {
        resultEater.addResult( "ControlFile", ctrlFile );

        log.log( false, " - Kontroll-Datei kopiert" );
      }
      else
        log.log( false, " - Kontroll-Datei fehlt" );

      final File beiwerteFile = new File( dathDir, "Beiwerte.aus" );
      if( beiwerteFile.exists() )
      {
        resultEater.addResult( "BeiwerteAus", beiwerteFile );
        log.log( false, " - Beiwerte-Datei kopiert" );
      }
      else
        log.log( false, " - Beiwerte-Datei fehlt" );

      final File lambdaFile = new File( dathDir, "lambda_i.txt" );
      if( lambdaFile.exists() )
      {
        resultEater.addResult( "LambdaI", lambdaFile );
        log.log( false, " - LambdaI-Datei kopiert" );
      }
      log.log( false, " - LambdaI-Datei fehlt" );

      if( log.checkCanceled() )
        return;

      final MODE calcMode = calculation.getCalcMode();
      if( log.checkCanceled() )
        return;

      final TuhhReach reach = calculation.getReach();

      switch( calcMode )
      {
        case WATERLEVEL:
        {
          final LengthSectionParser lsProcessor = new LengthSectionParser( reach, new File( dathDir, "laengsschnitt.txt" ), resultEater, tmpDir, epsThinning, false );
          final IStatus lsResult = lsProcessor.process( log );
          if( lsResult.getSeverity() == IStatus.ERROR )
          {
            log.log( false, " - Längsschnitt konnte nicht gelesen werden" );
            log.log( false, "Fehler bei der Berechnung. Bitte prüfen Sie die Ergebnis-Logs." );
            monitor.setFinishInfo( IStatus.ERROR, "Fehler bei der Berechnung. Bitte prüfen Sie die Ergebnis-Logs." );
            return;
          }

          if( log.checkCanceled() )
            return;

          final LengthSectionProcessor[] processedLengthSections = lsProcessor.getProcessedLengthSections();
          if( processedLengthSections.length < 1 )
          {
            log.log( true, "Fehler bei der Ergebnisauswertung. Bitte prüfen Sie die Ergebnis-Logs. ", new Object[0] );
            monitor.setFinishInfo( IStatus.ERROR, "Fehler bei der Ergebnisauswertung. Bitte prüfen Sie die Ergebnis-Logs. " );
            return;
          }
          else if( processedLengthSections.length > 1 )
          {
            log.log( true, "Fehler bei der Ergebnisauswertung. Bitte prüfen Sie die Ergebnis-Logs. Eventuell existieren Stationsrücksprünge entlang des Gewässers (z.B. infolge interpolierter Brückenprofile).", new Object[0] );
            monitor.setFinishInfo( IStatus.ERROR, "Fehler bei der Ergebnisauswertung. Bitte prüfen Sie die Ergebnis-Logs. Eventuell existieren Stationsrücksprünge entlang des Gewässers (z.B. infolge interpolierter Brückenprofile)." );
            return;
          }
          final LengthSectionProcessor processedLS = processedLengthSections[0];

          final File gmlFile = processedLS.getGmlFile();
          if( gmlFile.exists() )
          {
            resultEater.addResult( "LengthSectionGml", gmlFile );
            log.log( false, " - Längsschnitt (als GML) erzeugt." );
          }

          final File diagFile = processedLS.getDiagFile();
          if( diagFile.exists() )
          {
            resultEater.addResult( "LengthSectionDiag", diagFile );
            log.log( false, " - Längsschnitt-Diagramm erzeugt." );
          }

          final File tableFile = processedLS.getTableFile();
          if( tableFile.exists() )
          {
            resultEater.addResult( "LengthSectionTab", tableFile );
            log.log( false, " - Tabelle erzeugt." );
          }

          final File breaklineFile = processedLS.getBreaklineFile();
          if( breaklineFile.exists() )
          {
            resultEater.addResult( "Bruchkanten", breaklineFile );
            log.log( false, " - Bruchkanten erzeugt." );
          }

          final File tinFile = processedLS.getTinFile();
          if( tinFile.exists() )
          {
            resultEater.addResult( "WspTin", tinFile );
            log.log( false, " - Wasserspiegel-Tin erzeugt." );
          }

          final File boundaryFile = processedLS.getBoundaryFile();
          if( boundaryFile.exists() )
          {
            resultEater.addResult( "Modellgrenzen", boundaryFile );
            log.log( false, " - Modellgrenzen erzeugt." );
          }

          final File waterlevelFile = processedLS.getWaterlevelFile();
          if( waterlevelFile.exists() )
          {
            resultEater.addResult( "Ueberschwemmungslinie", waterlevelFile );
            log.log( false, " - Überschwemmungslinie erzeugt." );
          }

          if( log.checkCanceled() )
            return;

          //
          // overview map (generated by token replacement) either from OVW_MAP_SPECIAL or OVW_MAP_GENERAL
          //
          // TODO: maybe move into LengthSectionProcessor?
          if( ovwMapURL != null )
          {
            mapContentStream = ovwMapURL.openStream();
            final String mapContent = IOUtils.toString( mapContentStream );
            final FeaturePath ftPath = reach.getFeature().getWorkspace().getFeaturepathForFeature( reach.getFeature() );
            final String newMapContent = mapContent.replaceAll( "%FID%", ftPath.toString() );
            final File mapFile = new File( tmpDir, "map.gmt" );
            mapWriter = new PrintWriter( new BufferedWriter( new FileWriter( mapFile ) ) );
            mapWriter.write( newMapContent );
            mapWriter.close();
            if( mapFile.exists() )
            {
              resultEater.addResult( "OvwMap", mapFile );
              log.log( false, " - Übersichtskarte erzeugt." );
            }
          }

          if( log.checkCanceled() )
            return;

          // *.tab (-> fixed name "Ergebnis.list")
          final FileFilter ergListFilter = FileFilterUtils.suffixFileFilter( ".tab" );
          final File[] ergListFile = dathDir.listFiles( ergListFilter );
          if( ergListFile.length > 0 )
          {
            // assumption: max. one TAB-file
            resultEater.addResult( "resultList", ergListFile[0] );

            log.log( false, " - Ergebnisliste erzeugt." );
          }

          break;
        }

        case BF_UNIFORM:
        {
          // bankfull uniform
          // *.qb2 = Q als Treppenfunktion, we don't fetch it
          // *.qb1 = bankfull-lengthsection
          final FileFilter qb1Filter = FileFilterUtils.suffixFileFilter( ".qb1" );
          final File[] bfLenSecFile = dathDir.listFiles( qb1Filter );
          if( bfLenSecFile.length > 0 )
          {
            // assumption: max. one QB1
            resultEater.addResult( "bfLengthSection", bfLenSecFile[0] );
            log.log( false, " - Längsschnitt (bordvoll) erzeugt (*.qb1)." );
          }

          // *.tab (-> fixed name "Ergebnis.list")
          final FileFilter ergListFilter = FileFilterUtils.suffixFileFilter( ".tab" );
          final File[] ergListFile = dathDir.listFiles( ergListFilter );
          if( ergListFile.length > 0 )
          {
            // assumption: max. one TAB-file
            resultEater.addResult( "resultList", ergListFile[0] );
            log.log( false, " - Ergebnisliste erzeugt." );
          }

          break;
        }

        case BF_NON_UNIFORM:
        {
          // bankfull nonuniform

          // TODO: also parse QLang-File?

          // TODO :check everything below if it still makes sense?

          // *.qb2 = Q als Treppenfunktion, we don't fetch it
          // *.qb1 = bankfull-lengthsection
          final FileFilter qb1Filter = FileFilterUtils.suffixFileFilter( ".qb1" );
          final File[] bfLenSecFile = dathDir.listFiles( qb1Filter );
          if( bfLenSecFile.length > 0 )
          {
            // assumption: max. one QB1
            resultEater.addResult( "bfLengthSection", bfLenSecFile[0] );
            log.log( false, " - Längsschnitt (bordvoll) erzeugt (*.qb1)." );
          }

          /* break */
          // FALL THROUGH
        }

        case REIB_KONST:
        {
          /* Process length sections */
          /* Parse Q_LangSchnitt.txt into several length-sections */
          final File lsOutDir = new File( tmpDir, "LengthSections" );
          lsOutDir.mkdirs();
          resultEater.addResult( "resultListsNonUni", lsOutDir );

          final LengthSectionParser lsProcessor = new LengthSectionParser( reach, new File( dathDir, "Q_LangSchnitt.txt" ), resultEater, lsOutDir, epsThinning, true );
          final IStatus lsResult = lsProcessor.process( log );
          if( lsResult.getSeverity() == IStatus.ERROR )
          {
            log.log( false, " - Längsschnitte konnten nicht gelesen werden" );
            log.log( false, "Fehler bei der Berechnung. Bitte prüfen Sie die Ergebnis-Logs." );
            monitor.setFinishInfo( IStatus.ERROR, "Fehler bei der Berechnung. Bitte prüfen Sie die Ergebnis-Logs." );
            return;
          }

          log.log( false, " - Ergebnislisten pro Abfluss erzeugt." );
          if( log.checkCanceled() )
            return;

          if( log.checkCanceled() )
            return;

          /* Calculate and fetch Polynomes */
          final File polynomeTmpDir = new File( tmpDir, "polynome" );
          PolynomeHelper.processPolynomes( polynomeTmpDir, dathDir, log, 0, resultEater, calculation );

          break;
        }
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new SimulationException( "Fehler bei der Berechnung", e );
    }
    finally
    {
      IOUtils.closeQuietly( pwSimuLog );
      IOUtils.closeQuietly( zipInputStream );
      IOUtils.closeQuietly( strmKernelErr );
      IOUtils.closeQuietly( mapContentStream );
      IOUtils.closeQuietly( mapWriter );
    }
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#getSpezifikation()
   */
  public URL getSpezifikation( )
  {
    return getClass().getResource( CALCJOB_SPEC );
  }
}
