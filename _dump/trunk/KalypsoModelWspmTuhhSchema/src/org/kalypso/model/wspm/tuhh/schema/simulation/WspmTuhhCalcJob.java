/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
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
import org.kalypso.simulation.core.util.LogHelper;
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
  public static final int PROCESS_TIMEOUT = 10 * 60 * 1000;

  /**
   * @see org.kalypso.simulation.core.ISimulation#run(java.io.File, org.kalypso.simulation.core.ISimulationDataProvider,
   *      org.kalypso.simulation.core.ISimulationResultEater, org.kalypso.simulation.core.ISimulationMonitor)
   */
  public void run( final File tmpDir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final long lTimeout;
    if( inputProvider.hasID( "TIMEOUT" ) )
    {
      final Integer timeoutMins = (Integer) inputProvider.getInputForID( "TIMEOUT" );
      lTimeout = timeoutMins == null ? PROCESS_TIMEOUT : timeoutMins.intValue() * 60 * 1000;
    }
    else
      lTimeout = PROCESS_TIMEOUT;

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

      final LogHelper log = new LogHelper( pwSimuLog, monitor );
      log.log( true, "Parsing GMLXPath: %s", calcXPath );
      
      final GMLXPath calcpath = new GMLXPath( calcXPath );

      // load gml
      log.log( true, "Loading GML: %s", modellGmlURL );

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( modellGmlURL, null );

      // get calculation via path
      log.log( true, "Loading Calculation: %s", calcXPath );

      final Object calcObject = GMLXPathUtilities.query( calcpath, workspace );
      if( !(calcObject instanceof Feature) )
      {
        monitor.setFinishInfo( IStatus.ERROR, "GMLXPath points to no feature: " + calcObject );
        return;
      }

      final Feature calculationFeature = (Feature) calcObject;
      final TuhhCalculation calculation = new TuhhCalculation( calculationFeature );

      monitor.setProgress( 10 );
      
      log.log( true, "Writing files for tuhh calculation-core..." );

      // write calculation to tmpDir
      WspWinExporter.writeForTuhhKernel( calculation, tmpDir );

      // ensure availability of DATH directory (for results)
      final File dathDir = new File( tmpDir, "dath" );
      dathDir.mkdirs();

      // unpack kernel into tmpDir
      zipInputStream = WspmTuhhCalcJob.class.getResourceAsStream( "resources/rechenkern.zip" );
      ZipUtilities.unzip( zipInputStream, tmpDir, false );

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
      pwInParams.println( "\"" + tmpDir.getAbsolutePath() + File.separator + "kalypso-1D.ini" + "\"" );

      pwInParams.close();

      // generate start.bat
      final File fleBat = new File( tmpDir, "start.bat" );
      PrintWriter pwBat = new PrintWriter( new BufferedWriter( new FileWriter( fleBat ) ) );
      pwBat.println( "\"" + tmpDir.getAbsolutePath() + File.separator + "Kalypso-1D.exe" + "\" < " + "\"" + fleInParams.getAbsolutePath() + "\"" );
      pwBat.close();
      String sCmd = "\"" + fleBat.getAbsolutePath() + "\"";

      monitor.setProgress( 20 );

      /* Start kalypso-1d.exe */
      log.log( true, "Executing calculation core..." );

      if( log.checkCanceled() )
        return;

      // start calculation
      ProcessHelper.startProcess( sCmd, null, tmpDir, monitor, lTimeout, strmKernelLog, strmKernelErr, null );
      
      if( log.checkCanceled() )
        return;

      // load results + copy to result folder + unzip templates
      monitor.setProgress( 80 );
      monitor.setMessage( "Retrieving results" );
      pwSimuLog.println( "Ergebnis-Dateien werden generiert und ¸bertragen." );

      // alle Modi
      final File ctrlFile = new File( dathDir, "Kontroll.log" );
      if( ctrlFile.exists() )
      {
        resultEater.addResult( "ControlFile", ctrlFile );
        
        log.log( false, " - Kontroll-Datei erzeugt." );
      }

      final File beiwerteFile = new File( dathDir, "Beiwerte.aus" );
      if( beiwerteFile.exists() )
      {
        resultEater.addResult( "BeiwerteAus", beiwerteFile );
        log.log( false,  " - Beiwerte-Datei erzeugt." );
      }

      final File lambdaFile = new File( dathDir, "lambda_i.txt" );
      if( lambdaFile.exists() )
      {
        resultEater.addResult( "LambdaI", lambdaFile );
        log.log( false,  " - LambdaI-Datei erzeugt." );
      }

      if( log.checkCanceled() )
        return;
      
      MODE calcMode = calculation.getCalcMode();
      switch( calcMode )
      {
        case WATERLEVEL:
        {
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
              log.log( false,  " - ‹bersichtskarte erzeugt." );
            }
          }

          if( log.checkCanceled() )
            return;

          //
          // laengsschnitt.txt
          //
          final File lenSecFile = new File( dathDir, "laengsschnitt.txt" );
          if( lenSecFile.exists() )
          {
            resultEater.addResult( "LengthSection", lenSecFile );
            log.log( false,  " - L‰ngsschnitt erzeugt." );

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
            strLengthSection = strLengthSection.replaceAll( "NaN", " -999.999 " );
            // strLengthSection = strLengthSection.replaceAll( "\\**" , " NaN " );

            // remove first two rows (old header) from laengsschnitt.txt
            int pos = strLengthSection.indexOf( "\n" );
            pos = strLengthSection.indexOf( "\n", pos + 1 );
            strLengthSection = org.apache.commons.lang.StringUtils.right( strLengthSection, strLengthSection.length() - pos );

            FileUtils.writeStringToFile( lengthSectionGmlFile, strHeader + strLengthSection + strFooter, WSPMTUHH_CODEPAGE );
            if( lengthSectionGmlFile.exists() )
            {
              resultEater.addResult( "LengthSectionGml", lengthSectionGmlFile );
              log.log( false,  " - L‰ngsschnitt (als GML) erzeugt." );

              // Read Length-Section GML
              final GMLWorkspace obsWks = GmlSerializer.createGMLWorkspace( lengthSectionGmlFile.toURL(), null );
              final Feature rootFeature = obsWks.getRootFeature();

              final IObservation<TupleResult> lengthSectionObs = ObservationFeatureFactory.toObservation( rootFeature );
              final TupleResult result = lengthSectionObs.getResult();
              final String strStationierung = "urn:ogc:gml:dict:kalypso:model:wspm:components#LengthSectionStation";
              final String strWsp = "urn:ogc:gml:dict:kalypso:model:wspm:components#LengthSectionWaterlevel";
              final TuhhReachProfileSegment[] reachProfileSegments = calculation.getReach().getReachProfileSegments();

              //
              // Diagramm
              //
              try
              {
                final File diagFile = new File( tmpDir, "L‰ngsschnitt.kod" );

                final WspmWaterBody waterBody = calculation.getReach().getWaterBody();
                LaengsschnittHelper.createDiagram( diagFile, lengthSectionObs, waterBody.isDirectionUpstreams() );
                if( diagFile.exists() )
                {
                  resultEater.addResult( "LengthSectionDiag", diagFile );
                  log.log( false,  " - L‰ngsschnitt-Diagramm erzeugt." );
                }
              }
              catch( final Exception e )
              {
                log.log( e, "L‰ngsschnitt-Diagramm konnte nicht erzeugt werden: %s", e.getLocalizedMessage() );
              }

              if( log.checkCanceled() )
                return;

              //
              // Table
              //
              try
              {
                final File tableFile = new File( tmpDir, "table.gft" );
                final URL tableUrl = getClass().getResource( "resources/table.gft" );
                FileUtilities.makeFileFromUrl( tableUrl, tableFile, false );

                resultEater.addResult( "LengthSectionTab", tableFile );
                log.log( false,  " - Tabelle erzeugt." );
              }
              catch( final Exception e )
              {
                log.log( e, "Tabelle konnte nicht erzeugt werden: %s", e.getLocalizedMessage() );
              }

              if( log.checkCanceled() )
                return;

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
                  log.log( false,  " - Bruchkanten erzeugt." );
                }
              }
              catch( final Exception e )
              {
                log.log( e, "Bruchkanten konnten nicht erzeugt werden: %s", e.getLocalizedMessage() );
              }

              if( log.checkCanceled() )
                return;

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
                  log.log( false,  " - Modellgrenzen erzeugt." );
                }
              }
              catch( final Exception e )
              {
                log.log( e, "Modellgrenzen konnten nicht erzeugt werden: %s", e.getLocalizedMessage() );
              }

              if( log.checkCanceled() )
                return;

              //
              // Waterlevel
              //
              try
              {
                final File file = new File( tmpDir, "‹berschwemmungslinie.gml" );
                BreakLinesHelper.createModelBoundary( reachProfileSegments, result, strStationierung, strWsp, file, true );
                if( file.exists() )
                {
                  resultEater.addResult( "Ueberschwemmungslinie", file );
                  log.log( false,  " - ‹berschwemmungslinie erzeugt." );
                }
              }
              catch( final Exception e )
              {
                log.log( e, "‹berschwemmungslinie konnte nicht erzeugt werden: %s", e.getLocalizedMessage() );
              }
              
              if( log.checkCanceled() )
                return;

            }
          }

          // *.tab (-> fixed name "Ergebnis.list")
          final FileFilter ergListFilter = FileFilterUtils.suffixFileFilter( ".tab" );
          final File[] ergListFile = dathDir.listFiles( ergListFilter );
          if( ergListFile.length > 0 )
          {
            // assumption: max. one TAB-file
            resultEater.addResult( "resultList", ergListFile[0] );
            
            log.log( false,  " - Ergebnisliste erzeugt." );
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
            log.log( false,  " - L‰ngsschnitt (bordvoll) erzeugt (*.qb1)." );
          }

          // *.tab (-> fixed name "Ergebnis.list")
          final FileFilter ergListFilter = FileFilterUtils.suffixFileFilter( ".tab" );
          final File[] ergListFile = dathDir.listFiles( ergListFilter );
          if( ergListFile.length > 0 )
          {
            // assumption: max. one TAB-file
            resultEater.addResult( "resultList", ergListFile[0] );
            log.log( false,  " - Ergebnisliste erzeugt." );
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
            log.log( false,  " - L‰ngsschnitt (bordvoll) erzeugt (*.qb1)." );
          }

          // *.km (W/Q-Tabelle f¸r Kalinin-Miljukow)
          final FileFilter kmFilter = FileFilterUtils.suffixFileFilter( ".km" );
          final File[] kmFiles = dathDir.listFiles( kmFilter );
          if( kmFiles.length > 0 )
          {
            // assumption: max. one KM
            resultEater.addResult( "wqKalMil", kmFiles[0] );
            log.log( false,  " - W/Q-Tabelle zur Bestimmung der Kalinin-Miljukow-Parameter erzeugt." );
          }

          // *.pro (W/Q-Tabellen f¸r jedes Profil -> Verzeichnis)
          final FileFilter wqProProfilFilter = FileFilterUtils.suffixFileFilter( ".pro" );
          final File[] proFiles = dathDir.listFiles( wqProProfilFilter );
          final File wqProProfilDir = new File( dathDir, "WQTabs" );
          wqProProfilDir.mkdirs();
          for( final File file : proFiles )
          {
            FileUtils.copyFileToDirectory( file, wqProProfilDir );
            
            if( log.checkCanceled() )
              return;
          }
          
          if( wqProProfilDir.exists() )
          {
            resultEater.addResult( "wqProProfil", wqProProfilDir );
            log.log( false,  " - W/Q-Tabelle pro Profil erzeugt." );
          }

          // *.tb (Ergebnislisten f¸r jeden Abfluss -> Verzeichnis)
          final FileFilter resNonUniFilter = FileFilterUtils.suffixFileFilter( ".tb" );
          final File[] resNonUniFiles = dathDir.listFiles( resNonUniFilter );
          final File resNonUniDir = new File( dathDir, "ResultLists" );
          resNonUniDir.mkdirs();
          for( final File file : resNonUniFiles )
            FileUtils.copyFileToDirectory( file, resNonUniDir );
          if( resNonUniDir.exists() )
          {
            resultEater.addResult( "resultListsNonUni", resNonUniDir );
            log.log( false,  " - Ergebnislisten pro Abfluss erzeugt." );
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
