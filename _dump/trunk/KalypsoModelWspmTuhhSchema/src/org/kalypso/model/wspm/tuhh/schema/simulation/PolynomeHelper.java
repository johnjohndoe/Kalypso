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
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.LineNumberReader;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Formatter;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.lang.ProcessHelper;
import org.kalypso.commons.java.lang.ProcessHelper.ProcessTimeoutException;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.java.io.filter.PrefixSuffixFilter;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.contribs.java.util.FormatterUtils;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.tuhh.core.gml.PolynomeProperties;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.gml.PolynomeProperties.Dreiteilung;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResult;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResultCollection;
import org.kalypso.model.wspm.tuhh.schema.schemata.IWspmTuhhQIntervallConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.phenomenon.IPhenomenon;
import org.kalypso.observation.phenomenon.Phenomenon;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.util.LogHelper;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * Helper class to start the processing of the polynomes.
 * 
 * @author Gernot Belger
 */
public class PolynomeHelper
{
  private static final String WEIR_FILE_NAME = "HOW_QWehr_HUW.txt";

  private static final String BRIDGE_FILE_NAME = "HOW_QBruecke_HUW.txt";

  private static final String QLANG_FILE_NAME = "Q_LangSchnitt.txt";

  /**
   * Prepares the input files for the polynome process
   * 
   * @param tmpDir
   *            any tmp dir, must be empty before start, may be deleted after end
   * @param dathDir
   *            Directory containing the laengsschnitt.txt and the beiwerte.aus files.
   * @return The polynom input dir (01Eingang), if preparation was succesful, else <code>null</code>.
   */
  private static File preparePolynomes( final File tmpDir, final File dathDir, final LogHelper log )
  {
    /* The files needed from the 1D-calculation */
    final File lsQFile = new File( dathDir, QLANG_FILE_NAME );
    final File weirFile = new File( dathDir, WEIR_FILE_NAME );
    final File bridgeFile = new File( dathDir, BRIDGE_FILE_NAME );

    final File[] dathFiles = new File[] { lsQFile, weirFile, bridgeFile };

    /* Check input data */
    for( final File file : dathFiles )
    {
      if( !file.exists() )
      {
        log.log( false, "Ergebnisdatei %s für Polynomerzeugung nicht vorhanden. Abbruch.", file );
        return null;
      }
    }

    /* Prepare exe dir */
    InputStream zipInputStream = null;
    try
    {
      zipInputStream = new BufferedInputStream( WspmTuhhCalcJob.class.getResourceAsStream( "resources/polynom1d.zip" ) );
      ZipUtilities.unzip( zipInputStream, tmpDir, false );
      zipInputStream.close();
    }
    catch( final IOException e )
    {
      log.log( e, "Fehler beim Entpacken der polynom1d.exe. Abbruch." );
      return null;
    }
    finally
    {
      IOUtils.closeQuietly( zipInputStream );
    }

    /* Copy input data to exe dir */
    try
    {
      final File eingangDir = new File( tmpDir, "01Eingang" );

      for( final File file : dathFiles )
        FileUtils.copyFileToDirectory( file, eingangDir );

      return eingangDir;
    }
    catch( final IOException e )
    {
      log.log( e, "Eingangsdaten für Polynomberechnung konnten nicht kopiert werden. Abbruch." );
      return null;
    }
  }

  public static void processPolynomes( final File tmpDir, final File dathDir, final LogHelper log, final long timeout, final ISimulationResultEater resultEater, final TuhhCalculation calculation ) throws SimulationException
  {
    final ISimulationMonitor monitor = log.getMonitor();

    log.log( true, "Polynomfuktionen werden ermittelt" );

    log.log( true, "- Übertrage Ergebnisse der Q-Intervallberechnung" );

    final File eingangDir = preparePolynomes( tmpDir, dathDir, log );
    final File resultDir = new File( tmpDir, "02Ausgang" );
    if( eingangDir == null )
      return;

    if( monitor.isCanceled() )
      return;

    log.log( true, "- Starte Polynome1d.exe" );
    prepareSteuerpoly( tmpDir, calculation );

    if( monitor.isCanceled() )
      return;

    final File logFile = new File( tmpDir, "Polynome1d.log" );
    final File errFile = new File( tmpDir, "Polynome1d.err" );

    OutputStream logStream = null;
    OutputStream errStream = null;
    try
    {
      logStream = new BufferedOutputStream( new FileOutputStream( logFile ) );
      errStream = new BufferedOutputStream( new FileOutputStream( errFile ) );

      /* Start the polynome1d process */
      final File exeFile = new File( tmpDir, "Polynome1d.exe" );
      final String cmdLine = "cmd.exe /C \"" + exeFile.getAbsolutePath() + "\"";
      ProcessHelper.startProcess( cmdLine, null, exeFile.getParentFile(), monitor, timeout, logStream, errStream, null );

      logStream.close();
      errStream.close();

      /* The weir and brigde files are not processed by the polynome.exe, just copy it to the result-folder */
      FileUtils.copyFileToDirectory( new File( eingangDir, WEIR_FILE_NAME ), resultDir );
      FileUtils.copyFileToDirectory( new File( eingangDir, BRIDGE_FILE_NAME ), resultDir );
    }
    catch( final IOException e )
    {
      log.log( e, "Fehler bei der Ausführung der Polynome1D.exe: %s" + e.getLocalizedMessage() );
      monitor.setFinishInfo( IStatus.ERROR, "Fehler bei der ausführung der Polynome1D.exe" );
      throw new SimulationException( "Fehler bei der Ausführung der Polynome1D.exe: %s" + e.getLocalizedMessage(), e );
    }
    catch( final ProcessTimeoutException e )
    {
      log.log( false, "Polynome1D-Prozess wurde abgebrochen. Grund: timeout" );
      monitor.setFinishInfo( IStatus.ERROR, "Polynome1D Prozess wurde abgebrochen. Grund: timeout" );
      return;
    }
    finally
    {
      IOUtils.closeQuietly( logStream );
      IOUtils.closeQuietly( errStream );
    }

    if( log.checkCanceled() )
      return;

    /* Read results */
    log.log( true, "- Lese Punktwolken und Polynome" );
    final File targetGmlFile = new File( tmpDir, "qIntervallResults.gml" );
    try
    {
      readResults( resultDir, targetGmlFile, calculation, log, resultEater );
      final File gmvResultFile = new File( tmpDir, "Ergebnisse.gmv" );
      resultEater.addResult( "qIntervallResultGmv", gmvResultFile );
    }
    catch( final Throwable e )
    {
      log.log( e, "Fehler beim Lesen der Polynom1D-Ergebnisse: %s", e.getLocalizedMessage() );
      monitor.setFinishInfo( IStatus.ERROR, "Fehler beim Lesen der Polynom1D-Ergebnisse: " + e.getLocalizedMessage() );
    }

    final File polynomeLogFile = new File( tmpDir, "Polynome1d.log" );
    if( polynomeLogFile.exists() )
      resultEater.addResult( "polynomeLog", polynomeLogFile );
  }

  private static void prepareSteuerpoly( final File tmpDir, final TuhhCalculation calculation ) throws SimulationException
  {
    final File steuerFile = new File( tmpDir, "steuerpoly.ini" );

    try
    {
      final double startStation = calculation.getStartStation().doubleValue();
      final double endStation = calculation.getStartStation().doubleValue();

      /* Polynomial Parameters */
      final PolynomeProperties pps = calculation.getPolynomeProperties();
      final int polynomialDeegree = pps.getDeegree();
      final boolean ignoreOutlier = pps.getIgnoreOutlier();
      final Dreiteilung triple = pps.getTriple();
      final boolean isTriple = triple != Dreiteilung.none;
      final double alphaLimit = pps.getAlphaLimit();
      final boolean autoSlopeDetection = isTriple && triple == Dreiteilung.slope;
      final BigDecimal runoffSlope = pps.getRunoffSlope();
      final BigDecimal areaSlope = pps.getAreaSlope();
      final BigDecimal alphaSlope = pps.getAlphaSlope();
      final BigDecimal weightSplinePoint = pps.getWeightSplinePoint();

      // Programming Language C (PRC) locale in order to format with decimal '.'
      final Formatter formatter = new Formatter( steuerFile, Charset.defaultCharset().name(), Locale.PRC );

      formatter.format( "Steuerdatei fuer die Polynomfunktionen je Profil%n" );
      formatter.format( "-------------------------------------------------%n" );
      formatter.format( "02 Längsschnitt(Pfad) 01Eingang\\%s%n", QLANG_FILE_NAME );
      formatter.format( "03 PolyGrad(2,3,4) %d%n", polynomialDeegree );
      formatter.format( "04 DreiTeil(J/N) %1s%n", isTriple ? "J" : "N" );
      formatter.format( "05 PolyReduce(J/N) J%n" );
      formatter.format( "06 ProfIntervall(J/N) N%n" );
      formatter.format( "07 StartProf(0000.0000) %.4f%n", startStation );
      formatter.format( "08 EndProf(0000.0000) %.4f%n", endStation );
      formatter.format( "09 AusgabeJeFunktion(J/N) J%n" );
      formatter.format( "10 AusgabeWspWerte(J/N) J%n" );
      formatter.format( "11 AusgabeKontrolle(J/N) J%n" );
      formatter.format( "12 AusgabeFile(Pfad) 02Ausgang\\%n" );
      formatter.format( "13 Ausreisser(J/N) %1s%n", ignoreOutlier ? "J" : "N" );
      formatter.format( "14 Impulsstrom(00.0000) %.4f%n", alphaLimit );
      formatter.format( "15 AutoSteigung(J/N) %1s%n", autoSlopeDetection ? "J" : "N" );
      formatter.format( "16 Q_Steigung(00.0000) %.4f%n", runoffSlope );
      formatter.format( "17 A_Steigung(00.0000) %.4f%n", areaSlope );
      formatter.format( "18 Alp_Steigung(00.0000) %.4f%n", alphaSlope );
      formatter.format( "19 WichtungSplinePkt(0000.00) %.2f%n", weightSplinePoint );

      formatter.close();

      FormatterUtils.checkIoException( formatter );
    }
    catch( final IOException e )
    {
      throw new SimulationException( "Could not write 'steuerpoly.ini'", e );
    }
    finally
    {
    }
  }

  private static void readResults( final File resultDir, final File targetGmlFile, final TuhhCalculation calculation, final LogHelper log, final ISimulationResultEater resultEater ) throws InvocationTargetException, IOException, GmlSerializeException, SimulationException
  {
    /* Read results */
    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( QIntervallResultCollection.QNAME_F_QIntervallResultCollection, targetGmlFile.toURL(), GmlSerializer.DEFAULT_FACTORY );
    final QIntervallResultCollection resultCollection = new QIntervallResultCollection( workspace.getRootFeature() );
    final Map<BigDecimal, QIntervallResult> pointResults = readProfFiles( resultDir, resultCollection, calculation, log );

    if( log.checkCanceled() )
      return;

    readPolynomeFile( resultDir, pointResults, log );
    readBuildingFile( new File( resultDir, WEIR_FILE_NAME ), pointResults, log );
    readBuildingFile( new File( resultDir, BRIDGE_FILE_NAME ), pointResults, log );

    if( log.checkCanceled() )
      return;

    /* Write workspace into file */
    GmlSerializer.serializeWorkspace( targetGmlFile, workspace, "CP1252" );
    resultEater.addResult( "qIntervallResultGml", targetGmlFile );
  }

  private static Map<BigDecimal, QIntervallResult> readProfFiles( final File resultDir, final QIntervallResultCollection resultCollection, final TuhhCalculation calculation, final LogHelper log )
  {
    final Map<BigDecimal, QIntervallResult> results = new HashMap<BigDecimal, QIntervallResult>();

    /* Read w-points first: PROFxxx.xxxx.txt files */
    final FilenameFilter filter = new PrefixSuffixFilter( "PROF", ".txt" );
    final File[] profFiles = resultDir.listFiles( filter );
    if( profFiles == null || profFiles.length == 0 )
    {
      log.finish( IStatus.ERROR, "Fehler beim Lesen der Polynom1D-Ergebnisse: keine PROFxxx.xx.txt Dateien vorhanden." );
      return results;
    }

    final SortedMap<BigDecimal, WspmProfile> profileIndex = indexProfiles( calculation );

    for( final File profFile : profFiles )
    {
      final String name = profFile.getName();
      if( name.length() < 9 )
        continue;

      final String stationString = name.substring( 4, name.length() - 4 );
      final BigDecimal station = new BigDecimal( stationString );

      // REMARK: as the slope is not nicely written to the polynome result files, we get it from the calculation.
      // BUT: this is only valid for the REIB_KONST mode! So maybe we should change something later...?
      final Double startSlope = calculation.getStartSlope();
      final BigDecimal slope = new BigDecimal( startSlope ).setScale( 5, RoundingMode.HALF_UP );

      try
      {
        final QIntervallResult qresult = resultCollection.createQResult();
        qresult.setName( station.toString() );
        qresult.setDescription( "Gelesen aus: " + name );
        qresult.setStation( station );
        qresult.setSlope( slope );

        /* Link to profile */
        final WspmProfile profile = profileForStation( profileIndex, station );
        if( profile != null )
          qresult.setProfileLink( profile );

        /* Create the points observation */
        final IObservation<TupleResult> observation = qresult.getPointsObservation();
        final String obsName = stationString;
        final String description = "Übernommen aus Datei: " + name;
        observation.setName( obsName );
        observation.setDescription( description );
        readProfFile( profFile, observation.getResult(), log );
        qresult.setPointsObservation( observation );

        results.put( station, qresult );
      }
      catch( final Exception e )
      {
        log.log( e, "Fehler beim Lesen einer PROF-Datei: ", name );
      }
    }

    return results;
  }

  private static WspmProfile profileForStation( final SortedMap<BigDecimal, WspmProfile> profileIndex, final BigDecimal station )
  {
    return (WspmProfile) forStation( profileIndex, station );
  }

  public static Object forStation( final SortedMap<BigDecimal, ? extends Object> stationIndex, final BigDecimal station )
  {
// final double delta = 0.00001;
// final BigDecimal pred = new BigDecimal( station.doubleValue() - delta );
// final BigDecimal succ = new BigDecimal( station.doubleValue() + delta );
    final BigDecimal pred = NumberUtils.decrement( station );
    final BigDecimal succ = NumberUtils.increment( pred );
    final SortedMap<BigDecimal, ? extends Object> subMap = stationIndex.subMap( pred, succ );
    if( !subMap.isEmpty() )
      return subMap.values().iterator().next();

    return stationIndex.get( station );
  }

  public static <S> S forStationAdjacent( final SortedMap<BigDecimal, S> stationIndex, final BigDecimal station, final boolean upstream )
  {
    final BigDecimal pred = NumberUtils.decrement( station );
    final BigDecimal succ = NumberUtils.increment( station );

    if( upstream )
    {
      final SortedMap<BigDecimal, ? extends Object> successors = stationIndex.tailMap( succ );
      if( !successors.isEmpty() )
        return stationIndex.get( successors.firstKey() );
    }
    else
    {

      final SortedMap<BigDecimal, ? extends Object> predecessors = stationIndex.headMap( pred );
      if( !predecessors.isEmpty() )
        return stationIndex.get( predecessors.lastKey() );
    }

    return null;
  }

  private static SortedMap<BigDecimal, WspmProfile> indexProfiles( final TuhhCalculation calculation )
  {
    final TuhhReach reach = calculation.getReach();
    final TuhhReachProfileSegment[] reachProfileSegments = reach.getReachProfileSegments();
    final SortedMap<BigDecimal, WspmProfile> index = new TreeMap<BigDecimal, WspmProfile>();
    for( final TuhhReachProfileSegment segment : reachProfileSegments )
    {
      final WspmProfile profileMember = segment.getProfileMember();
      index.put( segment.getStation(), profileMember );
    }

    return index;
  }

  private static TupleResult readProfFile( final File profFile, final TupleResult tupleResult, final LogHelper log ) throws IOException
  {
    final IComponent[] pointsComponents = tupleResult.getComponents();

    LineNumberReader reader = null;
    try
    {
      reader = new LineNumberReader( new FileReader( profFile ) );

      while( reader.ready() )
      {
        final String line = reader.readLine();
        if( line == null )
          break;

        final String[] tokens = line.trim().split( " +" );
        if( tokens == null || tokens.length != 8 )
          continue;

        /* Determine if this is a good line */
        final String firstToken = tokens[0].replace( 'D', 'E' );
        try
        {
          new Double( firstToken );
        }
        catch( final NumberFormatException nfe )
        {
          /* Just ignore this line */
          continue;
        }

        /* Do parse the line */
        final IRecord record = tupleResult.createRecord();
        for( int i = 0; i < tokens.length; i++ )
        {
          final String token = tokens[i].replace( 'D', 'E' );
          try
          {
            final BigDecimal value = new BigDecimal( token );
            record.setValue( pointsComponents[i], value );
          }
          catch( final NumberFormatException nfe )
          {
            /* A good line but bad content. Give user a hint that something might be wrong. */
            log.log( false, "Lesefehler in Datei: %s - Zeile: %d - Token: %s", profFile.getName(), reader.getLineNumber(), token );
          }
        }

        /* Only add if all values are ok */
        tupleResult.add( record );
      }

      reader.close();
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }

    return tupleResult;
  }

  private static void readPolynomeFile( final File resultDir, final Map<BigDecimal, QIntervallResult> pointResults, final LogHelper log ) throws IOException
  {
    final File polyFile = new File( resultDir, "Polynome.TXT" );

    LineNumberReader reader = null;
    try
    {
      reader = new LineNumberReader( new FileReader( polyFile ) );

      while( reader.ready() )
      {
        final String line = reader.readLine();
        if( line == null )
          break;

        final String trimmedLine = line.trim().replaceAll( " \\(h\\)", "\\(h\\)" );
        final String[] tokens = trimmedLine.split( " +" );
        if( tokens.length < 8 )
          continue;

        /* Determine if this is a good line: good lines are lines whos first token is a number */
        final String firstToken = tokens[0];
        try
        {
          new Double( firstToken );
        }
        catch( final NumberFormatException nfe )
        {
          /* Just ignore this line */
          continue;
        }

        try
        {
          final BigDecimal station = new BigDecimal( tokens[0] );
          final String description = tokens[1];
          // final String whatIsN = tokens[2];
          final char type = tokens[3].charAt( 0 );

          final int order = Integer.parseInt( tokens[4] );
          final double rangeMin = Double.parseDouble( tokens[5].replace( 'D', 'E' ) );
          final double rangeMax = Double.parseDouble( tokens[6].replace( 'D', 'E' ) );

          if( tokens.length < 7 + order + 1 )
          {
            /* A good line but bad content. Give user a hint that something might be wrong. */
            log.log( false, "Anzahl der Koeffizienten falsche in Datei: %s - Zeile: %d", polyFile.getName(), reader.getLineNumber() );
            continue;
          }

          final List<Double> coefficients = new ArrayList<Double>( order );
          for( int i = 7; i < 7 + order + 1; i++ )
          {
            final double coeff = Double.parseDouble( tokens[i].replace( 'D', 'E' ) );
            coefficients.add( coeff );
          }

          final Double[] doubles = coefficients.toArray( new Double[coefficients.size()] );
          final double[] coeffDoubles = ArrayUtils.toPrimitive( doubles );

          final String domainId;
          final String rangeId = IWspmTuhhQIntervallConstants.DICT_PHENOMENON_WATERLEVEL;
          switch( type )
          {
            case 'Q':
              domainId = IWspmTuhhQIntervallConstants.DICT_PHENOMENON_RUNOFF;
              break;
            case 'A':
              domainId = IWspmTuhhQIntervallConstants.DICT_PHENOMENON_AREA;
              break;
            case 'a':
              domainId = IWspmTuhhQIntervallConstants.DICT_PHENOMENON_ALPHA;
              break;

            default:
              log.log( false, "Unbekannter Wert-Typ '%c' in Zeile %s: %s ", station );
              continue;
          }

          /* find feature for station */
          final QIntervallResult qresult = pointResults.get( station );
          if( qresult == null )
            log.log( false, "Keine passende Station für Polynom bei km %.4f: %s", station, line );
          else
          {
            /* create new polynome */
            final IPolynomial1D poly1d = qresult.createPolynomial();

            poly1d.setName( description );
            poly1d.setDescription( description );
            poly1d.setCoefficients( coeffDoubles );
            poly1d.setRange( rangeMin, rangeMax );

            poly1d.setDomainPhenomenon( domainId );
            poly1d.setRangePhenomenon( rangeId );
          }
        }
        catch( final NumberFormatException nfe )
        {
          /* A good line but bad content. Give user a hint that something might be wrong. */
          log.log( false, "Lesefehler in Datei: %s - Zeile: %d: %s", polyFile.getName(), reader.getLineNumber(), nfe.getLocalizedMessage() );
        }
        catch( final Exception e )
        {
          // should never happen
          log.log( e, "Lesefehler in Datei: %s - Zeile: %d: %s", polyFile.getName(), reader.getLineNumber(), e.getLocalizedMessage() );
        }

      }
      reader.close();
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  /**
   * Reads the contents of 'HOW_QWehr_HUW.txt' into the qIntervallResults.
   */
  private static void readBuildingFile( final File buildingFile, final Map<BigDecimal, QIntervallResult> pointResults, final LogHelper log ) throws IOException
  {
    LineNumberReader reader = null;
    try
    {
      reader = new LineNumberReader( new FileReader( buildingFile ) );

      /* Ingore first line */
      if( reader.ready() )
        reader.readLine();

      while( reader.ready() )
      {
        final String line = reader.readLine();
        if( line == null )
          break;

        try
        {
          readBuildingLine( line.trim(), pointResults, buildingFile );
        }
        catch( final NumberFormatException nfe )
        {
          /* A good line but bad content. Give user a hint that something might be wrong. */
          log.log( false, "Lesefehler in Datei: %s - Zeile: %d: %s", buildingFile.getName(), reader.getLineNumber(), nfe.getLocalizedMessage() );
        }
        catch( final Throwable e )
        {
          // should never happen
          log.log( e, "Lesefehler in Datei: %s - Zeile: %d: %s", buildingFile.getName(), reader.getLineNumber(), e.getLocalizedMessage() );
        }

      }
      reader.close();
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  private static void readBuildingLine( final String line, final Map<BigDecimal, QIntervallResult> pointResults, final File buildingFile ) throws Exception
  {
    final String[] tokens = line.split( " +" );
    if( tokens.length < 6 )
      return;

    /* Determine if this is a good line: good lines are lines whos first token is a number */
    final String firstToken = tokens[0];
    try
    {
      new Double( firstToken );
    }
    catch( final NumberFormatException nfe )
    {
      /* Just ignore this line */
      return;
    }

    final BigDecimal station = new BigDecimal( tokens[0] );
    final BigDecimal qOW = new BigDecimal( tokens[1] );
    /* final BigDecimal qWehr = new BigDecimal( tokens[2] ); */
    final BigDecimal hOW = new BigDecimal( tokens[3] );
    final BigDecimal hUW = new BigDecimal( tokens[4] );
    /* final String ueArt = tokens[5]; */

    /* Find or create a result for this station */
    if( !pointResults.containsKey( station ) )
    {
      /* Create a new Point Result if not yet existant */
      // REMARK: hopefully, there is always at least one result
      final QIntervallResult firstqResult = pointResults.values().iterator().next();
      final QIntervallResultCollection qresultCollection = new QIntervallResultCollection( firstqResult.getFeature().getParent() );
      final QIntervallResult newqresult = qresultCollection.createQResult();
      newqresult.setStation( station );

      newqresult.setName( station.toString() );
      final String descMessage = "Gelesen aus: " + buildingFile.getName();
      if( !newqresult.getDescription().contains( descMessage ) )
        newqresult.setDescription( descMessage );

      pointResults.put( station, newqresult );
    }

    final QIntervallResult qresult = pointResults.get( station );

    /* Add comment */
    qresult.setDescription( qresult.getDescription() + "\nGelesen aus: " + buildingFile.getName() );

    /* Add values to the weir observation */
    final IObservation<TupleResult> weirObs = qresult.getBuildingObservation( true );

    final String buildingId = qresult.getBuildingId();
    if( buildingId != null )
    {
      /* Set the phenomenon of the building as phenomenon for the observation */
      final IPhenomenon buildingPhenomenon = new Phenomenon( buildingId, "", "" );
      weirObs.setPhenomenon( buildingPhenomenon );
    }

    final TupleResult result = weirObs.getResult();
    final IComponent[] components = result.getComponents();
    final IComponent compHOW = ComponentUtilities.findComponentByID( components, IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL_UPSTREAM );
    final IComponent compHUW = ComponentUtilities.findComponentByID( components, IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL_DOWNSTREAM );
    final IComponent compRunoff = ComponentUtilities.findComponentByID( components, IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF );

    // put extra result into observation
    final IRecord newRecord = result.createRecord();
    newRecord.setValue( compRunoff, qOW );
    newRecord.setValue( compHOW, hOW );
    newRecord.setValue( compHUW, hUW );
    result.add( newRecord );

    qresult.setWeirObservation( weirObs );
  }

}
