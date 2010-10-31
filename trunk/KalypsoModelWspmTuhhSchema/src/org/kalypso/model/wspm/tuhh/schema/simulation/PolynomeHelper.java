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

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.net.URL;
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
import org.kalypso.contribs.java.io.filter.PrefixSuffixFilter;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.contribs.java.util.FormatterUtils;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.gml.PolynomeProperties;
import org.kalypso.model.wspm.tuhh.core.gml.PolynomeProperties.TripleMode;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResult;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResultCollection;
import org.kalypso.model.wspm.tuhh.schema.i18n.Messages;
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
  private static final String ERGEBNISSE_GMV = "Ergebnisse.gmv"; //$NON-NLS-1$

  public static final String POLYNOME_1D_EXE_FORMAT = "Polynome1d%s.exe";//$NON-NLS-1$ 

  public static final String POLYNOME_1D_EXE_PATTERN = "Polynome1d(.*).exe";//$NON-NLS-1$ 

  // TODO: Deciding which approach to take, energy level or water stage

// private static final String WEIR_FILE_NAME = "HOW_QWehr_HUW.txt";
  private static final String WEIR_FILE_NAME = "EOW_QWehr_EUW.txt"; //$NON-NLS-1$

// private static final String BRIDGE_FILE_NAME = "HOW_QBruecke_HUW.txt";
  private static final String BRIDGE_FILE_NAME = "EOW_QBruecke_EUW.txt"; //$NON-NLS-1$

  private static final String QLANG_FILE_NAME = "Q_LangSchnitt.txt"; //$NON-NLS-1$

  /**
   * Prepares the input files for the polynome process
   * 
   * @param tmpDir
   *          any tmp dir, must be empty before start, may be deleted after end
   * @param dathDir
   *          Directory containing the laengsschnitt.txt and the beiwerte.aus files.
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
        log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.0" ), file ); //$NON-NLS-1$
        return null;
      }
    }

    /* Copy input data to exe dir */
    try
    {
      final File eingangDir = new File( tmpDir, "01Eingang" ); //$NON-NLS-1$

      for( final File file : dathFiles )
        FileUtils.copyFileToDirectory( file, eingangDir );

      return eingangDir;
    }
    catch( final IOException e )
    {
      log.log( e, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.2" ) ); //$NON-NLS-1$
      return null;
    }
  }

  public static void processPolynomes( final File tmpDir, final File dathDir, final LogHelper log, final long timeout, final ISimulationResultEater resultEater, final TuhhCalculation calculation ) throws SimulationException
  {
    final ISimulationMonitor monitor = log.getMonitor();

    log.log( true, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.3" ) ); //$NON-NLS-1$

    log.log( true, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.4" ) ); //$NON-NLS-1$

    final File eingangDir = preparePolynomes( tmpDir, dathDir, log );
    final File resultDir = new File( tmpDir, "02Ausgang" ); //$NON-NLS-1$
    /* Need to create result-dir, else the calculation does not work */
    resultDir.mkdirs();
    if( eingangDir == null )
      return;

    if( monitor.isCanceled() )
      return;

    log.log( true, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.5" ) ); //$NON-NLS-1$
    prepareSteuerpoly( tmpDir, calculation );

    if( monitor.isCanceled() )
      return;

    final File logFile = new File( tmpDir, "Polynome1d.log" ); //$NON-NLS-1$
    final File errFile = new File( tmpDir, "Polynome1d.err" ); //$NON-NLS-1$

    OutputStream logStream = null;
    OutputStream errStream = null;
    try
    {
      logStream = new BufferedOutputStream( new FileOutputStream( logFile ) );
      errStream = new BufferedOutputStream( new FileOutputStream( errFile ) );

      /* Start the polynome1d process */
      final File exeFile = WspmTuhhCalcJob.getExecuteable( calculation, tmpDir, POLYNOME_1D_EXE_FORMAT, POLYNOME_1D_EXE_PATTERN, monitor );
      if( exeFile == null )
        return;

      final String cmdLine = "cmd.exe /C \"" + exeFile.getAbsolutePath() + "\""; //$NON-NLS-1$ //$NON-NLS-2$
      ProcessHelper.startProcess( cmdLine, null, tmpDir, monitor, timeout, logStream, errStream, null );

      logStream.close();
      errStream.close();

      /* The weir and brigde files are not processed by the polynome.exe, just copy it to the result-folder */
      FileUtils.copyFileToDirectory( new File( eingangDir, WEIR_FILE_NAME ), resultDir );
      FileUtils.copyFileToDirectory( new File( eingangDir, BRIDGE_FILE_NAME ), resultDir );
    }
    catch( final IOException e )
    {
      log.log( e, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.8" ) + e.getLocalizedMessage() ); //$NON-NLS-1$
      monitor.setFinishInfo( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.9" ) ); //$NON-NLS-1$
      throw new SimulationException( Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.10" ) + e.getLocalizedMessage(), e ); //$NON-NLS-1$
    }
    catch( final ProcessTimeoutException e )
    {
      log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.11" ) ); //$NON-NLS-1$
      monitor.setFinishInfo( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.11" ) ); //$NON-NLS-1$
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
    log.log( true, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.13" ) ); //$NON-NLS-1$
    final File targetGmlFile = new File( tmpDir, "qIntervallResults.gml" ); //$NON-NLS-1$
    try
    {
      readResults( resultDir, targetGmlFile, calculation, log, resultEater );
      final File gmvResultFile = new File( tmpDir, ERGEBNISSE_GMV );
      final URL ergebnisseGmbLocation = PolynomeHelper.class.getResource( "resources/" + ERGEBNISSE_GMV ); //$NON-NLS-1$
      FileUtils.copyURLToFile( ergebnisseGmbLocation, gmvResultFile );
      resultEater.addResult( WspmTuhhCalcJob.OUTPUT_QINTERVALL_RESULT_GMV, gmvResultFile );
    }
    catch( final Throwable e )
    {
      log.log( e, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.14" ), e.getLocalizedMessage() ); //$NON-NLS-1$
      monitor.setFinishInfo( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.15" ) + e.getLocalizedMessage() ); //$NON-NLS-1$
    }

    final File polynomeLogFile = new File( tmpDir, "Polynome1d.log" ); //$NON-NLS-1$
    if( polynomeLogFile.exists() )
      resultEater.addResult( "polynomeLog", polynomeLogFile ); //$NON-NLS-1$
  }

  private static void prepareSteuerpoly( final File tmpDir, final TuhhCalculation calculation ) throws SimulationException
  {
    final File steuerFile = new File( tmpDir, "steuerpoly.ini" ); //$NON-NLS-1$

    try
    {
      final double startStation = calculation.getStartStation().doubleValue();
      final double endStation = calculation.getStartStation().doubleValue();

      /* Polynomial Parameters */
      final PolynomeProperties pps = calculation.getPolynomeProperties();
      final int polynomialDeegree = pps.getDeegree();
      final boolean ignoreOutlier = pps.getIgnoreOutlier();
      final boolean isTripleForAll = pps.getTripleForAll();
      final BigDecimal alphaLimit = pps.getAlphaLimit();
      final TripleMode tripleMode = pps.getTripleMode();
      final boolean autoSlopeDetection = tripleMode == TripleMode.slopeChange;
      final BigDecimal runoffSlope = pps.getRunoffSlope();
      final BigDecimal areaSlope = pps.getAreaSlope();
      final BigDecimal alphaSlope = pps.getAlphaSlope();
      final BigDecimal weightSplinePoint = pps.getWeightSplinePoint();

      // Programming Language C (PRC) locale in order to format with decimal '.'
      final Formatter formatter = new Formatter( steuerFile, Charset.defaultCharset().name(), Locale.PRC );

      formatter.format( "Steuerdatei fuer die Polynomfunktionen je Profil%n" ); //$NON-NLS-1$
      formatter.format( "-------------------------------------------------%n" ); //$NON-NLS-1$
      formatter.format( "02 L‰ngsschnitt(Pfad) 01Eingang\\%s%n", QLANG_FILE_NAME ); //$NON-NLS-1$
      formatter.format( "03 PolyGrad(2,3,4) %d%n", polynomialDeegree ); //$NON-NLS-1$
      formatter.format( "04 DreiTeil(J/N) %1s%n", isTripleForAll ? "J" : "N" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      formatter.format( "05 PolyReduce(J/N) J%n" ); //$NON-NLS-1$
      formatter.format( "06 ProfIntervall(J/N) N%n" ); //$NON-NLS-1$
      formatter.format( "07 StartProf(0000.0000) %.4f%n", startStation ); //$NON-NLS-1$
      formatter.format( "08 EndProf(0000.0000) %.4f%n", endStation ); //$NON-NLS-1$
      formatter.format( "09 AusgabeJeFunktion(J/N) J%n" ); //$NON-NLS-1$
      formatter.format( "10 AusgabeWspWerte(J/N) J%n" ); //$NON-NLS-1$
      formatter.format( "11 AusgabeKontrolle(J/N) J%n" ); //$NON-NLS-1$
      formatter.format( "12 AusgabeFile(Pfad) 02Ausgang\\%n" ); //$NON-NLS-1$
      formatter.format( "13 Ausreisser(J/N) %1s%n", ignoreOutlier ? "J" : "N" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      formatter.format( "14 Impulsstrom(00.0000) %.4f%n", alphaLimit ); //$NON-NLS-1$
      formatter.format( "15 AutoSteigung(J/N) %1s%n", autoSlopeDetection ? "J" : "N" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      formatter.format( "16 Q_Steigung(00.0000) %.4f%n", runoffSlope ); //$NON-NLS-1$
      formatter.format( "17 A_Steigung(00.0000) %.4f%n", areaSlope ); //$NON-NLS-1$
      formatter.format( "18 Alp_Steigung(00.0000) %.4f%n", alphaSlope ); //$NON-NLS-1$
      formatter.format( "19 WichtungSplinePkt(0000.00) %.2f%n", weightSplinePoint ); //$NON-NLS-1$

      formatter.close();

      FormatterUtils.checkIoException( formatter );
    }
    catch( final IOException e )
    {
      throw new SimulationException( Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.16" ), e ); //$NON-NLS-1$
    }
    finally
    {
    }
  }

  private static void readResults( final File resultDir, final File targetGmlFile, final TuhhCalculation calculation, final LogHelper log, final ISimulationResultEater resultEater ) throws IOException, GmlSerializeException, SimulationException, GMLSchemaException
  {
    /* Read results */
    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( QIntervallResultCollection.QNAME_F_QIntervallResultCollection, targetGmlFile.toURI().toURL(), GmlSerializer.DEFAULT_FACTORY );
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
    GmlSerializer.serializeWorkspace( targetGmlFile, workspace, "UTF-8" ); //$NON-NLS-1$
    resultEater.addResult( WspmTuhhCalcJob.OUTPUT_QINTERVALL_RESULT, targetGmlFile );
  }

  private static Map<BigDecimal, QIntervallResult> readProfFiles( final File resultDir, final QIntervallResultCollection resultCollection, final TuhhCalculation calculation, final LogHelper log )
  {
    final Map<BigDecimal, QIntervallResult> results = new HashMap<BigDecimal, QIntervallResult>();

    /* Read w-points first: PROFxxx.xxxx.txt files */
    final FilenameFilter filter = new PrefixSuffixFilter( "PROF", ".txt" ); //$NON-NLS-1$ //$NON-NLS-2$
    final File[] profFiles = resultDir.listFiles( filter );
    if( profFiles == null || profFiles.length == 0 )
    {
      log.finish( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.17" ) ); //$NON-NLS-1$
      return results;
    }

    final SortedMap<BigDecimal, IProfileFeature> profileIndex = indexProfiles( calculation );

    for( final File profFile : profFiles )
    {
      final String name = profFile.getName();
      if( name.length() < 9 )
        continue;

      final String stationString = name.substring( 4, name.length() - 4 );
      final BigDecimal station = new BigDecimal( stationString );

      // REMARK: as the slope is not nicely written to the polynome result files, we get it from the calculation.
      // BUT: this is only valid for the REIB_KONST mode! So maybe we should change something later...?
      final BigDecimal startSlope = calculation.getStartSlope();
      final BigDecimal slope = startSlope.setScale( 5, RoundingMode.HALF_UP );

      try
      {
        final QIntervallResult qresult = resultCollection.createQResult();
        qresult.setName( station.toString() );
        qresult.setDescription( Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.18" ) + name ); //$NON-NLS-1$
        qresult.setStation( station );
        qresult.setSlope( slope );

        /* Link to profile */
        final IProfileFeature profile = profileForStation( profileIndex, station );
        if( profile != null )
          qresult.setProfileLink( profile );

        /* Create the points observation */
        final IObservation<TupleResult> observation = qresult.getPointsObservation();
        final String obsName = stationString;
        final String description = Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.19" ) + name; //$NON-NLS-1$
        observation.setName( obsName );
        observation.setDescription( description );
        readProfFile( profFile, observation.getResult(), log );
        qresult.setPointsObservation( observation );

        results.put( station, qresult );
      }
      catch( final Exception e )
      {
        log.log( e, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.20" ), name ); //$NON-NLS-1$
      }
    }

    return results;
  }

  private static IProfileFeature profileForStation( final SortedMap<BigDecimal, IProfileFeature> profileIndex, final BigDecimal station )
  {
    return (IProfileFeature) forStation( profileIndex, station );
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

  private static SortedMap<BigDecimal, IProfileFeature> indexProfiles( final TuhhCalculation calculation )
  {
    final TuhhReach reach = calculation.getReach();
    final TuhhReachProfileSegment[] reachProfileSegments = reach.getReachProfileSegments();
    final SortedMap<BigDecimal, IProfileFeature> index = new TreeMap<BigDecimal, IProfileFeature>();
    for( final TuhhReachProfileSegment segment : reachProfileSegments )
    {
      final IProfileFeature profileMember = segment.getProfileMember();
      index.put( segment.getStation(), profileMember );
    }

    return index;
  }

  private static TupleResult readProfFile( final File profFile, final TupleResult tupleResult, final LogHelper log ) throws IOException
  {
    LineNumberReader reader = null;
    try
    {
      reader = new LineNumberReader( new FileReader( profFile ) );

      while( reader.ready() )
      {
        final String line = reader.readLine();
        if( line == null )
          break;

        final String[] tokens = line.trim().split( " +" ); //$NON-NLS-1$
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
            record.setValue( i, value );
          }
          catch( final NumberFormatException nfe )
          {
            /* A good line but bad content. Give user a hint that something might be wrong. */
            log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.21" ), profFile.getName(), reader.getLineNumber(), token ); //$NON-NLS-1$
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
    final File polyFile = new File( resultDir, "Polynome.TXT" ); //$NON-NLS-1$

    LineNumberReader reader = null;
    try
    {
      reader = new LineNumberReader( new FileReader( polyFile ) );

      while( reader.ready() )
      {
        final String line = reader.readLine();
        if( line == null )
          break;

        final String trimmedLine = line.trim().replaceAll( " \\(h\\)", "\\(h\\)" ); //$NON-NLS-1$ //$NON-NLS-2$
        final String[] tokens = trimmedLine.split( " +" ); //$NON-NLS-1$
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
            log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.22" ), polyFile.getName(), reader.getLineNumber() ); //$NON-NLS-1$
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
              log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.23" ), station ); //$NON-NLS-1$
              continue;
          }

          /* find feature for station */
          final QIntervallResult qresult = pointResults.get( station );
          if( qresult == null )
            log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.24" ), station, line ); //$NON-NLS-1$
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
          log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.25" ), polyFile.getName(), reader.getLineNumber(), nfe.getLocalizedMessage() ); //$NON-NLS-1$
        }
        catch( final Exception e )
        {
          // should never happen
          log.log( e, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.25" ), polyFile.getName(), reader.getLineNumber(), e.getLocalizedMessage() ); //$NON-NLS-1$
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
          log.log( false, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.25" ), buildingFile.getName(), reader.getLineNumber(), nfe.getLocalizedMessage() ); //$NON-NLS-1$
        }
        catch( final Throwable e )
        {
          // should never happen
          log.log( e, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.25" ), buildingFile.getName(), reader.getLineNumber(), e.getLocalizedMessage() ); //$NON-NLS-1$
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
    final String[] tokens = line.split( " +" ); //$NON-NLS-1$
    if( tokens.length < 6 )
      return;

    /* Determine if this is a good line: good lines are lines whose first token is a number */
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
      final QIntervallResultCollection qresultCollection = new QIntervallResultCollection( firstqResult.getFeature().getOwner() );
      final QIntervallResult newqresult = qresultCollection.createQResult();
      newqresult.setStation( station );

      newqresult.setName( station.toString() );
      final String descMessage = Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.26" ) + buildingFile.getName(); //$NON-NLS-1$
      if( !newqresult.getDescription().contains( descMessage ) )
        newqresult.setDescription( descMessage );

      pointResults.put( station, newqresult );
    }

    final QIntervallResult qresult = pointResults.get( station );

    /* Add comment */
    qresult.setDescription( qresult.getDescription() + Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.27" ) + buildingFile.getName() ); //$NON-NLS-1$

    /* Add values to the weir observation */
    final IObservation<TupleResult> weirObs = qresult.getBuildingObservation( true );

    final String buildingId = qresult.getBuildingId();
    if( buildingId != null )
    {
      /* Set the phenomenon of the building as phenomenon for the observation */
      final IPhenomenon buildingPhenomenon = new Phenomenon( buildingId, "", "" ); //$NON-NLS-1$ //$NON-NLS-2$
      weirObs.setPhenomenon( buildingPhenomenon );
    }

    final TupleResult result = weirObs.getResult();
    final IComponent[] components = result.getComponents();
    final IComponent compHOW = ComponentUtilities.findComponentByID( components, IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL_UPSTREAM );
    final IComponent compHUW = ComponentUtilities.findComponentByID( components, IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL_DOWNSTREAM );
    final IComponent compRunoff = ComponentUtilities.findComponentByID( components, IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF );

    // put extra result into observation
    final IRecord newRecord = result.createRecord();
    newRecord.setValue( result.indexOfComponent( compRunoff ), qOW );
    newRecord.setValue( result.indexOfComponent( compHOW ), hOW );
    newRecord.setValue( result.indexOfComponent( compHUW ), hUW );
    result.add( newRecord );

    qresult.setWeirObservation( weirObs );
  }

}
