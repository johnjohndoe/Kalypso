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

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileFilter;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.PrintWriter;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.model.wspm.core.gml.WspmReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation.MODE;
import org.kalypso.model.wspm.tuhh.core.wspwin.WspWinExporter;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeaturePath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;

/**
 * @author thuel2
 */
public class WspmTuhhCalcJob implements ISimulation
{
  public static final String CALCJOB_SPEC = "WspmTuhhCalcJob_spec.xml";

  public static final String WSPMTUHH_CODEPAGE = "Cp1252";

  // Timeout beim Rechnen([ms])
  public static final int PROCESS_TIMEOUT = 600000;

  public static final String MESS_BERECHNUNG_ABGEBROCHEN = "Modell: Berechnung abgebrochen";

  public WspmTuhhCalcJob( )
  {
    // will not be instantiated
  }

  /**
   * @see org.kalypso.simulation.core.ISimulation#run(java.io.File, org.kalypso.simulation.core.ISimulationDataProvider,
   *      org.kalypso.simulation.core.ISimulationResultEater, org.kalypso.simulation.core.ISimulationMonitor)
   */
  public void run( final File tmpDir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    long lTimeout = PROCESS_TIMEOUT;
    final URL modellGmlURL = (URL) inputProvider.getInputForID( "MODELL_GML" );
    final String calcXPath = (String) inputProvider.getInputForID( "CALC_PATH" );
    final String epsThinning = (String) inputProvider.getInputForID( "EPS_THINNING" );
    URL ovwMapURL = null;
    if( inputProvider.hasID( "OVW_MAP_GENERAL" ) )
      ovwMapURL = (URL) inputProvider.getInputForID( "OVW_MAP_GENERAL" );
    if( inputProvider.hasID( "OVW_MAP_SPECIAL" ) )
      ovwMapURL = (URL) inputProvider.getInputForID( "OVW_MAP_SPECIAL" );

    final File simulogFile = new File( tmpDir, "simulation.log" );
    resultEater.addResult( "SimulationLog", simulogFile );

    PrintWriter pwSimuLog = null;
    InputStream zipInputStream = null;
    InputStream headerInputStream = null;
    InputStream footerInputStream = null;
    FileOutputStream strmKernelLog = null;
    FileOutputStream strmKernelErr = null;
    PrintWriter pwInParams = null;
    InputStream mapContentStream = null;
    PrintWriter mapWriter = null;
    try
    {
      pwSimuLog = new PrintWriter( new BufferedWriter( new FileWriter( simulogFile ) ) );

      pwSimuLog.println( "Parsing GMLXPath: " + calcXPath );
      monitor.setMessage( "Parsing GMLXPath: " + calcXPath );
      final GMLXPath calcpath = new GMLXPath( calcXPath );

      // load gml
      pwSimuLog.println( "Loading GML: " + modellGmlURL );
      monitor.setMessage( "Loading GML: " + modellGmlURL );
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( modellGmlURL );

      // get calculation via path
      pwSimuLog.println( "Loading Calculation: " + calcXPath );
      monitor.setMessage( "Loading Calculation: " + calcXPath );

      final Object calcObject = GMLXPathUtilities.query( calcpath, workspace );
      if( !(calcObject instanceof Feature) )
      {
        monitor.setFinishInfo( IStatus.ERROR, "GMLXPath points to no feature: " + calcObject );
        return;
      }

      final Feature calculationFeature = (Feature) calcObject;
      final TuhhCalculation calculation = new TuhhCalculation( calculationFeature );

      monitor.setProgress( 10 );
      pwSimuLog.println( "Writing files for tuhh-mode" );
      monitor.setMessage( "Writing kernel data" );

      // write calculation to tmpDir
      WspWinExporter.writeForTuhhKernel( calculation, tmpDir );

      // ensure availability of DATH directory (for results)
      final File dathDir = new File( tmpDir, "dath" );
      dathDir.mkdirs();

      // unpack kernel into tmpDir
      zipInputStream = WspmTuhhCalcJob.class.getResourceAsStream( "resources/rechenkern.zip" );
      ZipUtilities.unzip( zipInputStream, tmpDir, false );

      pwSimuLog.println( "Log-Dateien werden übertragen." );
      // prepare kernel logs (log and err)
      final File fleKernelLog = new File( tmpDir, "kernel.log" );
      resultEater.addResult( "KernelLog", fleKernelLog );
      strmKernelLog = new FileOutputStream( fleKernelLog );

      final File fleKernelErr = new File( tmpDir, "kernel.err" );
      resultEater.addResult( "KernelErr", fleKernelErr );
      strmKernelErr = new FileOutputStream( fleKernelErr );

      // TODO input.txt und start.bat sollen noch raus (nach Umstellung des Rechenkerns durch Wolf)
      // input.txt: n, prof/calc.properties
      final File fleInParams = new File( tmpDir, "input.txt" );
      pwInParams = new PrintWriter( new BufferedWriter( new FileWriter( fleInParams ) ) );
      pwInParams.println( "n" );
      pwInParams.println( tmpDir.getAbsolutePath() + File.separator + "kalypso-1D.ini" );

      pwInParams.close();

      // generate start.bat
      final File fleBat = new File( tmpDir, "start.bat" );
      PrintWriter pwBat = new PrintWriter( new BufferedWriter( new FileWriter( fleBat ) ) );
      pwBat.println( tmpDir.getAbsolutePath() + File.separator + "Kalypso-1D.exe < " + fleInParams.getAbsolutePath() );
      pwBat.close();
      String sCmd = fleBat.getAbsolutePath();

      // String sCmd = "Kalypso-1D.exe";

      monitor.setProgress( 20 );
      monitor.setMessage( "Executing model" );

      new String();
      if( monitor.isCanceled() )
      {
        pwSimuLog.println( MESS_BERECHNUNG_ABGEBROCHEN );
      }

      // start calculation
      ProcessHelper.startProcess( sCmd, null, tmpDir, monitor, lTimeout, strmKernelLog, strmKernelErr, null );
      if( monitor.isCanceled() )
      {
        pwSimuLog.println( MESS_BERECHNUNG_ABGEBROCHEN );
      }

      // load results + copy to result folder + unzip templates
      monitor.setProgress( 80 );
      monitor.setMessage( "Retrieving results" );
      pwSimuLog.println( "Ergebnis-Dateien werden generiert und übertragen." );

      // alle Modi
      final File ctrlFile = new File( dathDir, "Kontroll.log" );
      if( ctrlFile.exists() )
      {
        resultEater.addResult( "ControlFile", ctrlFile );
        pwSimuLog.println( " - Kontroll-Datei erzeugt." );
      }

      final File beiwerteFile = new File( dathDir, "Beiwerte.aus" );
      if( beiwerteFile.exists() )
      {
        resultEater.addResult( "BeiwerteAus", beiwerteFile );
        pwSimuLog.println( " - Beiwerte-Datei erzeugt." );
      }

      final File lambdaFile = new File( dathDir, "lambda_i.txt" );
      if( lambdaFile.exists() )
      {
        resultEater.addResult( "LambdaI", lambdaFile );
        pwSimuLog.println( " - LambdaI-Datei erzeugt." );
      }

      MODE calcMode = calculation.getCalcMode();
      switch( calcMode )
      {
        case WATERLEVEL:
        {
          // TODO return LengthSectionDiag, LengthSectionTab
          // waterlevel-Mode
          // *.wsl ignored (part of laengsschnitt.txt)

          //
          // overview map (generated by token replacement) either from OVW_MAP_SPECIAL or OVW_MAP_GENERAL
          //
          if( ovwMapURL != null )
          {
            mapContentStream = ovwMapURL.openStream();
            final String mapContent = IOUtils.toString( mapContentStream );
            final FeaturePath ftPath = calculation.getReach().getFeature().getWorkspace().getFeaturepathForFeature( calculation.getReach().getFeature() );
            final String newMapContent = mapContent.replaceAll( "%FID%", ftPath.toString() );
            final File mapFile = new File( tmpDir, "map.gmt" );
            mapWriter = new PrintWriter( new BufferedWriter( new FileWriter( mapFile ) ) );
            mapWriter.write( newMapContent );
            mapWriter.close();
            if( mapFile.exists() )
            {
              resultEater.addResult( "OvwMap", mapFile );
              pwSimuLog.println( " - Übersichtskarte erzeugt." );
            }
          }

          //
          // laengsschnitt.txt
          //
          final File lenSecFile = new File( dathDir, "laengsschnitt.txt" );
          if( lenSecFile.exists() )
          {
            resultEater.addResult( "LengthSection", lenSecFile );
            pwSimuLog.println( " - Längsschnitt erzeugt." );

            // process lenghtsection to result observation (laengsschnitt.gml): concatenate new header + laengsschnitt
            // (without header) + new footer
            File lengthSectionGmlFile = new File( tmpDir, "lengthSectionGml.gml" );

            headerInputStream = getClass().getResourceAsStream( "resources/headerLenghSection.txt" );
            footerInputStream = getClass().getResourceAsStream( "resources/footerLenghSection.txt" );

            // Info: gml header file contains same encoding
            final String strHeader = IOUtils.toString( headerInputStream, WSPMTUHH_CODEPAGE );
            final String strFooter = IOUtils.toString( footerInputStream, WSPMTUHH_CODEPAGE );
            String strLengthSection = FileUtils.readFileToString( lenSecFile, WSPMTUHH_CODEPAGE );
            
            // introduce space around 'NaN' and '***' values to make it parseable
            strLengthSection = strLengthSection.replaceAll( "NaN" , " -999.999 " );
//            strLengthSection = strLengthSection.replaceAll( "\\**" , " NaN " );
            
            // remove first two rows (old header) from laengsschnitt.txt
            int pos = strLengthSection.indexOf( "\n" );
            pos = strLengthSection.indexOf( "\n", pos + 1 );
            strLengthSection = org.apache.commons.lang.StringUtils.right( strLengthSection, strLengthSection.length() - pos );

            FileUtils.writeStringToFile( lengthSectionGmlFile, strHeader + strLengthSection + strFooter, WSPMTUHH_CODEPAGE );
            if( lengthSectionGmlFile.exists() )
            {
              resultEater.addResult( "LengthSectionGml", lengthSectionGmlFile );
              pwSimuLog.println( " - Längsschnitt (als GML) erzeugt." );

              // Read Length-Section GML
              final GMLWorkspace obsWks = GmlSerializer.createGMLWorkspace( lengthSectionGmlFile.toURL() );
              final Feature rootFeature = obsWks.getRootFeature();

              final IObservation<TupleResult> lengthSectionObs = ObservationFeatureFactory.toObservation( rootFeature );
              final TupleResult result = lengthSectionObs.getResult();
              final String strStationierung = "Stationierung";
              final String strWsp = "Höhe WSP";
              final WspmReachProfileSegment[] reachProfileSegments = calculation.getReach().getReachProfileSegments();

              //
              // Breaklines
              //
              try
              {
                final File breaklineFile = new File( tmpDir, "Bruchkanten.gml" );
                BreakLinesHelper.createBreaklines( reachProfileSegments, result, strStationierung, strWsp, Double.valueOf( epsThinning ), breaklineFile );
                if( breaklineFile.exists() )
                {
                  resultEater.addResult( "Bruchkanten", breaklineFile );
                  pwSimuLog.println( " - Bruchkanten erzeugt." );
                }
              }
              catch( final Exception e )
              {
                pwSimuLog.println( "Bruchkanten konnten nicht erzeugt werden: " + e.getLocalizedMessage() );
              }

              //
              // Model-Boundaries
              //
              try
              {
                final File file = new File( tmpDir, "Modellgrenzen.gml" );
                BreakLinesHelper.createModelBoundary( reachProfileSegments, result, strStationierung, strWsp, file, false );
                if( file.exists() )
                {
                  resultEater.addResult( "Modellgrenzen", file );
                  pwSimuLog.println( " - Modellgrenzen erzeugt." );
                }
              }
              catch( final Exception e )
              {
                pwSimuLog.println( "Modellgrenzen konnten nicht erzeugt werden: " + e.getLocalizedMessage() );
              }

              //
              // Waterlevel
              //
              try
              {
                final File file = new File( tmpDir, "Überschwemmungslinie.gml" );
                BreakLinesHelper.createModelBoundary( reachProfileSegments, result, strStationierung, strWsp, file, true );
                if( file.exists() )
                {
                  resultEater.addResult( "Ueberschwemmungslinie", file );
                  pwSimuLog.println( " - Überschwemmungslinie erzeugt." );
                }
              }
              catch( final Exception e )
              {
                pwSimuLog.println( "Überschwemmungslinie konnte nicht erzeugt werden: " + e.getLocalizedMessage() );
              }
            }
          }

          // *.tab (-> fixed name "Ergebnis.list")
          final FileFilter ergListFilter = FileFilterUtils.suffixFileFilter( ".tab" );
          final File[] ergListFile = dathDir.listFiles( ergListFilter );
          if( ergListFile.length > 0 )
          {
            // assumption: max. one TAB-file
            resultEater.addResult( "resultList", ergListFile[0] );
            pwSimuLog.println( " - Ergebnisliste erzeugt." );
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
            pwSimuLog.println( " - Längsschnitt (bordvoll) erzeugt (*.qb1)." );
          }

          // *.tab (-> fixed name "Ergebnis.list")
          final FileFilter ergListFilter = FileFilterUtils.suffixFileFilter( ".tab" );
          final File[] ergListFile = dathDir.listFiles( ergListFilter );
          if( ergListFile.length > 0 )
          {
            // assumption: max. one TAB-file
            resultEater.addResult( "resultList", ergListFile[0] );
            pwSimuLog.println( " - Ergebnisliste erzeugt." );
          }

          break;
        }
        case BF_NON_UNIFORM:
        {
          // bankfull nonuniform
          // *.qb2 = Q als Treppenfunktion, we don't fetch it
          // *.qb1 = bankfull-lengthsection
          final FileFilter qb1Filter = FileFilterUtils.suffixFileFilter( ".qb1" );
          final File[] bfLenSecFile = dathDir.listFiles( qb1Filter );
          if( bfLenSecFile.length > 0 )
          {
            // assumption: max. one QB1
            resultEater.addResult( "bfLengthSection", bfLenSecFile[0] );
            pwSimuLog.println( " - Längsschnitt (bordvoll) erzeugt (*.qb1)." );
          }

          // *.km (W/Q-Tabelle für Kalinin-Miljukow)
          final FileFilter kmFilter = FileFilterUtils.suffixFileFilter( ".km" );
          final File[] kmFiles = dathDir.listFiles( kmFilter );
          if( kmFiles.length > 0 )
          {
            // assumption: max. one KM
            resultEater.addResult( "wqKalMil", kmFiles[0] );
            pwSimuLog.println( " - W/Q-Tabelle zur Bestimmung der Kalinin-Miljukow-Parameter erzeugt." );
          }

          // *.pro (W/Q-Tabellen für jedes Profil -> Verzeichnis)
          final FileFilter wqProProfilFilter = FileFilterUtils.suffixFileFilter( ".pro" );
          final File[] proFiles = dathDir.listFiles( wqProProfilFilter );
          final File wqProProfilDir = new File( dathDir, "WQTabs" );
          wqProProfilDir.mkdirs();
          for( final File file : proFiles )
            FileUtils.copyFileToDirectory( file, wqProProfilDir );
          if( wqProProfilDir.exists() )
          {
            resultEater.addResult( "wqProProfil", wqProProfilDir );
            pwSimuLog.println( " - W/Q-Tabelle pro Profil erzeugt." );
          }

          // *.tb (Ergebnislisten für jeden Abfluss -> Verzeichnis)
          final FileFilter resNonUniFilter = FileFilterUtils.suffixFileFilter( ".tb" );
          final File[] resNonUniFiles = dathDir.listFiles( resNonUniFilter );
          final File resNonUniDir = new File( dathDir, "ResultLists" );
          resNonUniDir.mkdirs();
          for( final File file : resNonUniFiles )
            FileUtils.copyFileToDirectory( file, resNonUniDir );
          if( resNonUniDir.exists() )
          {
            resultEater.addResult( "resultListsNonUni", resNonUniDir );
            pwSimuLog.println( " - Ergebnislisten pro Abfluss erzeugt." );
          }
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
      IOUtils.closeQuietly( headerInputStream );
      IOUtils.closeQuietly( footerInputStream );
      IOUtils.closeQuietly( strmKernelLog );
      IOUtils.closeQuietly( strmKernelErr );
      IOUtils.closeQuietly( pwInParams );
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
