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
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.LineNumberReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
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
import org.kalypso.commons.metadata.MetadataObject;
import org.kalypso.contribs.java.io.filter.PrefixSuffixFilter;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.schema.schemata.IWspmTuhhQIntervallConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.util.LogHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.feature.binding.NamedFeatureHelper;

/**
 * Helper class to start the processing of the polynomes.
 * 
 * @author Gernot Belger
 */
public class PolynomeHelper
{

  /**
   * Prepares the input files for the polynome process
   * 
   * @param tmpDir
   *          any tmp dir, must be empty before start, may be deleted after end
   * @param dathDir
   *          Directory containing the laengsschnitt.txt and the beiwerte.aus files.
   * @return true, if preparation was succesful
   */
  private static boolean preparePolynomes( final File tmpDir, final File dathDir, final LogHelper log )
  {
    final File lsFile = new File( dathDir, "laengsschnitt.txt" );
    final File ausFile = new File( dathDir, "Beiwerte.AUS" );

    /* Check input data */
    if( !lsFile.exists() )
    {
      log.log( false, "Ergebnisdatei %s für Polynomerzeugung nicht vorhanden. Abbruch.", lsFile );
      return false;
    }

    if( !ausFile.exists() )
    {
      log.log( false, "Ergebnisdatei %s für Polynomerzeugung nicht vorhanden. Abbruch.", ausFile );
      return false;
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
      return false;
    }
    finally
    {
      IOUtils.closeQuietly( zipInputStream );
    }

    /* Copy input data to exe dir */
    try
    {
      final File eingangDir = new File( tmpDir, "01Eingang" );
      FileUtils.copyFileToDirectory( lsFile, eingangDir );
      FileUtils.copyFileToDirectory( ausFile, eingangDir );
    }
    catch( final IOException e )
    {
      log.log( e, "Eingangsdaten für Polynomberechnung konnten nicht kopiert werden. Abbruch." );
      return false;
    }
    return true;
  }

  public static void processPolynomes( final File tmpDir, final File dathDir, final LogHelper log, final long timeout, final ISimulationResultEater resultEater, final TuhhCalculation calculation ) throws SimulationException
  {
    final ISimulationMonitor monitor = log.getMonitor();

    log.log( true, "Polynomfuktionen werden ermittelt" );

    log.log( true, "- Übertrage Ergebnisse der Q-Intervallberechnung" );
    if( !preparePolynomes( tmpDir, dathDir, log ) )
      return;

    if( monitor.isCanceled() )
      return;

    log.log( true, "- Starte Polynome1d.exe" );
    prepareSteuerpoly( tmpDir, calculation );

    if( monitor.isCanceled() )
      return;

    /* Start the polynome1d process */
    final File exeFile = new File( tmpDir, "Polynome1d.exe" );
    final String cmdLine = "cmd.exe /C \"" + exeFile.getAbsolutePath() + "\"";

    final File logFile = new File( tmpDir, "Polynome1d.log" );
    // resultEater.addResult( "Polynome1DLog", logFile );
    final File errFile = new File( tmpDir, "Polynome1d.err" );
    // resultEater.addResult( "Polynome1DErr", errFile );

    OutputStream logStream = null;
    OutputStream errStream = null;
    try
    {
      logStream = new BufferedOutputStream( new FileOutputStream( logFile ) );
      errStream = new BufferedOutputStream( new FileOutputStream( errFile ) );

      ProcessHelper.startProcess( cmdLine, null, exeFile.getParentFile(), monitor, timeout, logStream, errStream, null );

      logStream.close();
      errStream.close();
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
    final File resultDir = new File( tmpDir, "02Ausgang" );
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
      return;
    }
  }

  private static void prepareSteuerpoly( final File tmpDir, final TuhhCalculation calculation ) throws SimulationException
  {
    final File steuerFile = new File( tmpDir, "steuerpoly.ini" );

    PrintWriter pw = null;

    try
    {
      final double startStation = calculation.getStartStation().doubleValue();
      final double endStation = calculation.getStartStation().doubleValue();

      // TODO: fetch other parameters from calculation
      pw = new PrintWriter( new BufferedWriter( new OutputStreamWriter( new FileOutputStream( steuerFile ) ) ) );
      pw.println( "Steuerdatei fuer die Polynomfunktionen je Profil" );
      pw.println( "-------------------------------------------------" );
      pw.println( "01 Beiwerte(Pfad) 01Eingang\\Beiwerte.AUS " );
      pw.println( "02 Längsschnitt(Pfad) 01Eingang\\laengsschnitt.txt" );
      pw.println( "03 PolyGrad(2,3,4) " + calculation.getPolynomialDeegree() );
      final String tripleIt = calculation.isPolynomialTriple() ? "J" : "N";
      pw.println( "04 DreiTeil(J/N) " + tripleIt );
      pw.println( "05 PolyReduce(J/N) J" );
      pw.println( "06 ProfIntervall(J/N) N" );
      pw.printf( Locale.PRC, "07 StartProf(0000.0000) %.4f", startStation );
      pw.println();
      pw.printf( Locale.PRC, "08 EndProf(0000.0000) %.4f", endStation );
      pw.println();
      pw.println( "09 AusgabeJeFunktion(J/N) J" );
      pw.println( "10 AusgabeWspWerte(J/N) J" );
      pw.println( "11 AusgabeKontrolle(J/N) J" );
      pw.println( "12 AusgabeFile(Pfad) 02Ausgang\\" );
    }
    catch( final FileNotFoundException e )
    {
      throw new SimulationException( "Could not write 'steuerpoly.ini'", e );
    }
    finally
    {
      IOUtils.closeQuietly( pw );
    }
  }

  private static void readResults( final File resultDir, final File targetGmlFile, final TuhhCalculation calculation, final LogHelper log, final ISimulationResultEater resultEater ) throws InvocationTargetException, IOException, GmlSerializeException, SimulationException
  {
    /* Read results */
    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( IWspmTuhhQIntervallConstants.QNAME_F_QIntervallResultCollection, targetGmlFile.toURL(), GmlSerializer.DEFAULT_FACTORY );
    final Feature resultCollectionFeature = workspace.getRootFeature();
    final Map<BigDecimal, Feature> pointResults = readProfFiles( resultDir, resultCollectionFeature, calculation, log );

    if( log.checkCanceled() )
      return;

    readPolynomeFile( resultDir, pointResults, log );

    if( log.checkCanceled() )
      return;

    /* Write workspace into file */
    GmlSerializer.serializeWorkspace( targetGmlFile, workspace, "CP1252" );
    resultEater.addResult( "qIntervallResultGml", targetGmlFile );
  }

  private static Map<BigDecimal, Feature> readProfFiles( final File resultDir, final Feature resultCollectionFeature, final TuhhCalculation calculation, final LogHelper log )
  {
    final GMLWorkspace workspace = resultCollectionFeature.getWorkspace();
    final IGMLSchema schema = workspace.getGMLSchema();
    final IFeatureType ftQIntervallResult = schema.getFeatureType( IWspmTuhhQIntervallConstants.QNAME_F_QIntervallResult );
    final IFeatureType ftObservation = schema.getFeatureType( IWspmTuhhQIntervallConstants.QNAME_F_WPointsObservation );
    final IFeatureType ftProfile = schema.getFeatureType( WspmProfile.QNAME_PROFILE );

    final IRelationType resultRelation = (IRelationType) resultCollectionFeature.getFeatureType().getProperty( IWspmTuhhQIntervallConstants.QNAME_P_QIntervallResultCollection_resultMember );

    final IRelationType pointsObsRelation = (IRelationType) ftQIntervallResult.getProperty( IWspmTuhhQIntervallConstants.QNAME_P_QIntervallResult_pointsMember );
    final IRelationType profileRelation = (IRelationType) ftQIntervallResult.getProperty( IWspmTuhhQIntervallConstants.QNAME_P_QIntervallResult_profileMember );

    final Map<BigDecimal, Feature> results = new HashMap<BigDecimal, Feature>();

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
        final Feature resultFeature = workspace.createFeature( resultCollectionFeature, resultRelation, ftQIntervallResult );
        resultCollectionFeature.getWorkspace().addFeatureAsComposition( resultCollectionFeature, resultRelation, -1, resultFeature );

        NamedFeatureHelper.setName( resultFeature, stationString );
        NamedFeatureHelper.setDescription( resultFeature, "Gelesen aus: " + name );

        resultFeature.setProperty( IWspmTuhhQIntervallConstants.QNAME_P_QIntervallResult_station, station );
        resultFeature.setProperty( IWspmTuhhQIntervallConstants.QNAME_P_QIntervallResult_slope, slope );

        final Feature obsFeature = workspace.createFeature( resultFeature, pointsObsRelation, ftObservation );
        resultFeature.setProperty( IWspmTuhhQIntervallConstants.QNAME_P_QIntervallResult_pointsMember, obsFeature );

        final WspmProfile profile = profileForStation( profileIndex, station );

        if( profile != null )
        {
          final String href = "project:/modell.gml#" + profile.getGmlID();
          final Feature profileFeatureRef = new XLinkedFeature_Impl( resultFeature, profileRelation, ftProfile, href, "", "", "", "", "" );
          resultFeature.setProperty( profileRelation, profileFeatureRef );
        }

        final IComponent[] pointsComponents = createPointsComponents( obsFeature );

        final TupleResult tupleResult = readProfFile( profFile, pointsComponents, log );
        final String obsName = stationString;
        final String description = "Übernommen aus Datei: " + name;
        final IObservation<TupleResult> obs = new Observation<TupleResult>( obsName, description, tupleResult, new ArrayList<MetadataObject>() );
        ObservationFeatureFactory.toFeature( obs, obsFeature );

        results.put( station, obsFeature );
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

  public static Object forStation( final SortedMap<BigDecimal, ? extends Object> profileIndex, final BigDecimal station )
  {
    final double delta = 0.00001;
    final BigDecimal pred = new BigDecimal( station.doubleValue() - delta );
    final BigDecimal succ = new BigDecimal( station.doubleValue() + delta );
    final SortedMap<BigDecimal, ? extends Object> subMap = profileIndex.subMap( pred, succ );
    if( !subMap.isEmpty() )
      return subMap.values().iterator().next();

    return profileIndex.get( station );
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

  private static TupleResult readProfFile( final File profFile, final IComponent[] pointsComponents, final LogHelper log ) throws IOException
  {
    final TupleResult tupleResult = new TupleResult( pointsComponents );

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

  private static IComponent[] createPointsComponents( final Feature obsFeature )
  {
    final IComponent[] components = new IComponent[8];

    components[0] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, "urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Waterlevel" );
    components[1] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, "urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Depth" );
    components[2] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, "urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Area" );
    components[3] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, "urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Runoff" );
    components[4] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, "urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#Alpha" );
    components[5] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, "urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaArea" );
    components[6] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, "urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaRunoff" );
    components[7] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, "urn:ogc:gml:dict:kalypso:model:wspmtuhh:qIntervallPointsComponents#DeltaAlpha" );

    return components;
  }

  private static void readPolynomeFile( final File resultDir, final Map<BigDecimal, Feature> pointResults, final LogHelper log ) throws IOException
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
          final String rangeId = IWspmTuhhQIntervallConstants.DICT_COMPONENT_WATERLEVEL;
          switch( type )
          {
            case 'Q':
              domainId = IWspmTuhhQIntervallConstants.DICT_COMPONENT_RUNOFF;
              break;
            case 'A':
              domainId = IWspmTuhhQIntervallConstants.DICT_COMPONENT_AREA;
              break;
            case 'a':
              domainId = IWspmTuhhQIntervallConstants.DICT_COMPONENT_ALPHA;
              break;

            default:
              log.log( false, "Unbekannter Wert-Typ '%c' in Zeile %s: %s ", station );
              continue;
          }

          /* find feature for station */
          final Feature pointsFeature = pointResults.get( station );
          if( pointsFeature == null )
            log.log( false, "Keine passende Station für Polynom bei km %.4f: %s", station, line );
          else
          {
            final Feature resultFeature = pointsFeature.getParent();
            final GMLWorkspace workspace = pointsFeature.getWorkspace();
            final IGMLSchema schema = workspace.getGMLSchema();
            final IFeatureType resultFT = schema.getFeatureType( IWspmTuhhQIntervallConstants.QNAME_F_QIntervallResult );
            final IRelationType polynomialRelation = (IRelationType) resultFT.getProperty( IWspmTuhhQIntervallConstants.QNAME_P_QIntervallResult_polynomialMember );

            final IFeatureType polynomialFT = schema.getFeatureType( IPolynomial1D.QNAME );

            /* create new polynome */
            final Feature polynomialFeature = workspace.createFeature( resultFeature, polynomialRelation, polynomialFT );
            workspace.addFeatureAsComposition( resultFeature, polynomialRelation, -1, polynomialFeature );
            final IPolynomial1D poly1d = (IPolynomial1D) polynomialFeature.getAdapter( IPolynomial1D.class );
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

}
