/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.commons.metadata.MetadataObject;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.java.io.filter.PrefixSuffixFilter;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.tuhh.schema.schemata.IWspmTuhhQIntervallConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.FeatureComponent;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypso.simulation.core.util.LogHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.binding.NamedFeatureHelper;

/**
 * Helper clas to start the processing of the polynomes.
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
    // TODO: comment in
    // if( !lsFile.exists() )
    // {
    // log.log( false, "Ergebnisdatei %s f�r Polynonerzeugung nicht vorhanden. Abbruch.", lsFile );
    // return false;
    // }
    //
    // if( !ausFile.exists() )
    // {
    // log.log( false, "Ergebnisdatei %s f�r Polynonerzeugung nicht vorhanden. Abbruch.", ausFile );
    // return false;
    // }
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
    // TODO: comment in
    // try
    // {
    // final File eingangDir = new File( tmpDir, "01Eingang" );
    // FileUtils.copyFileToDirectory( lsFile, eingangDir );
    // FileUtils.copyFileToDirectory( ausFile, eingangDir );
    // }
    // catch( final IOException e )
    // {
    // log.log( e, "Eingangsdaten f�r Polynomberechnung konnten nicht kopiert werden. Abbruch." );
    // return false;
    // }
    return true;
  }

  public static void processPolynomes( final File tmpDir, final File dathDir, final LogHelper log, final long timeout, final ISimulationResultEater resultEater ) throws SimulationException
  {
    if( !preparePolynomes( tmpDir, dathDir, log ) )
      return;

    final ISimulationMonitor monitor = log.getMonitor();
    if( monitor.isCanceled() )
      return;

    /* Start the polynome1d process */
    final File exeFile = new File( tmpDir, "Polynome1d.exe" );
    final String cmdLine = exeFile.getAbsolutePath();

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

      // TODO: comment in
      // ProcessHelper.startProcess( cmdLine, null, exeFile.getParentFile(), monitor, timeout, logStream, errStream,
      // null );

      logStream.close();
      errStream.close();
    }
    catch( final IOException e )
    {
      log.log( e, "Fehler bei der Ausf�hrung der Polynome1D.exe: %s" + e.getLocalizedMessage() );
      monitor.setFinishInfo( IStatus.ERROR, "Fehler bei der ausf�hrung der Polynome1D.exe" );
      return;
    }
    // TODO: comment in
    // catch( final ProcessTimeoutException e )
    // {
    // log.log( false, "Polynome1D-Prozess wurde abgebrochen. Grund: timeout" );
    // monitor.setFinishInfo( IStatus.ERROR, "Polynome1D Prozess wurde abgebrochen. Grund: timeout" );
    // return;
    // }
    finally
    {
      IOUtils.closeQuietly( logStream );
      IOUtils.closeQuietly( errStream );
    }

    if( log.checkCanceled() )
      return;

    final File resultDir = new File( tmpDir, "02Ausgang" );
    final File targetGmlFile = new File( tmpDir, "qIntervallResults.gml" );
    try
    {
      readResults( resultDir, targetGmlFile, log );
    }
    catch( MalformedURLException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( InvocationTargetException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( final IOException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( GmlSerializeException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  private static void readResults( final File resultDir, final File targetGmlFile, final LogHelper log ) throws InvocationTargetException, IOException, GmlSerializeException
  {
    /* Read results */
    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( IWspmTuhhQIntervallConstants.QNAME_F_QIntervallResultCollection, targetGmlFile.toURL(), GmlSerializer.DEFAULT_FACTORY );
    final Feature resultCollectionFeature = workspace.getRootFeature();
    final Map<Double, Feature> pointResults = readProfFiles( resultDir, resultCollectionFeature, log );

    if( log.checkCanceled() )
      return;

    /* Write workspace into file */
    GmlSerializer.serializeWorkspace( targetGmlFile, workspace, "UTF-8" );
  }

  private static Map<Double, Feature> readProfFiles( final File resultDir, final Feature resultCollectionFeature, final LogHelper log )
  {
    final GMLWorkspace workspace = resultCollectionFeature.getWorkspace();
    final IGMLSchema schema = workspace.getGMLSchema();
    final IFeatureType ftQIntervallResult = schema.getFeatureType( IWspmTuhhQIntervallConstants.QNAME_F_QIntervallResult );
    final IFeatureType ftObservation = schema.getFeatureType( new QName( NS.OM, "Observation" ) );

    final IRelationType resultRelation = (IRelationType) resultCollectionFeature.getFeatureType().getProperty( IWspmTuhhQIntervallConstants.QNAME_P_QIntervallResultCollection_resultMember );

    final IRelationType pointsObsRelation = (IRelationType) ftQIntervallResult.getProperty( IWspmTuhhQIntervallConstants.QNAME_P_QIntervallResult_pointsMember );

    final Map<Double, Feature> results = new HashMap<Double, Feature>();

    /* Read w-points first: PROFxxx.xxxx.txt files */
    final FilenameFilter filter = new PrefixSuffixFilter( "PROF", ".txt" );
    final File[] profFiles = resultDir.listFiles( filter );
    if( profFiles == null )
      // TODO error message
      return results;

    for( final File profFile : profFiles )
    {
      final String name = profFile.getName();
      if( name.length() != 14 )
        continue;

      final String stationString = name.substring( 4, 10 );
      final BigDecimal station = new BigDecimal( stationString );

      try
      {
        final Feature resultFeature = workspace.createFeature( resultCollectionFeature, resultRelation, ftQIntervallResult );
        resultCollectionFeature.getWorkspace().addFeatureAsComposition( resultCollectionFeature, resultRelation, -1, resultFeature );

        NamedFeatureHelper.setName( resultFeature, stationString );
        NamedFeatureHelper.setDescription( resultFeature, "Gelesen aus: " + name );

        resultFeature.setProperty( IWspmTuhhQIntervallConstants.QNAME_P_QIntervallResult_station, station );
        
        final Feature obsFeature = workspace.createFeature( resultFeature, pointsObsRelation, ftObservation );
        resultFeature.setProperty( IWspmTuhhQIntervallConstants.QNAME_P_QIntervallResult_pointsMember, obsFeature );

        final IComponent[] pointsComponents = createPointsComponents( obsFeature );

        final TupleResult tupleResult = readProfFile( profFile, pointsComponents, log );
        final String obsName = stationString;
        final String description = "�bernommen aus Datei: " + name;
        final IObservation<TupleResult> obs = new Observation<TupleResult>( obsName, description, tupleResult, new ArrayList<MetadataObject>() );
        ObservationFeatureFactory.toFeature( obs, obsFeature );

        results.put( station, obsFeature );
      }
      catch( final IOException e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
      catch( Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }

    return results;
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
            log.log( false, "Lesefehler in Datei: %s - Line: %d - Token: %s", profFile.getName(), reader.getLineNumber(), token );
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
}
