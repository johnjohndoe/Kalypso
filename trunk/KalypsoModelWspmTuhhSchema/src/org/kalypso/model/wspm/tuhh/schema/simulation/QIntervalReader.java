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

import java.io.File;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

import org.apache.commons.io.IOCase;
import org.apache.commons.io.IOUtils;
import org.apache.commons.io.filefilter.WildcardFileFilter;
import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
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
import org.kalypso.simulation.core.util.LogHelper;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * @author Gernot Belger
 */
public class QIntervalReader
{

  private final File m_inputDir;

  private final File m_targetGmlFile;

  private final TuhhCalculation m_calculation;

  private final LogHelper m_log;

  private String m_weirFilename;

  private String m_bridgeFilename;

  public QIntervalReader( final File inputDir, final File targetGmlFile, final TuhhCalculation calculation, final LogHelper log )
  {
    m_inputDir = inputDir;
    m_targetGmlFile = targetGmlFile;
    m_calculation = calculation;
    m_log = log;
  }

  public void setWeirFilename( final String weirFilename )
  {
    m_weirFilename = weirFilename;
  }

  public void setBridgeFilename( final String bridgeFilename )
  {
    m_bridgeFilename = bridgeFilename;
  }

  public void execute( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      readResults();
    }
    catch( final Exception e )
    {
      // TODO Auto-generated method stub
      e.printStackTrace();
      ProgressUtilities.done( monitor );
    }
  }

  private void readResults( ) throws IOException, GmlSerializeException, GMLSchemaException
  {
    /* Read results */
    final URL gmlContext = m_targetGmlFile.toURI().toURL();
    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( QIntervallResultCollection.QNAME_F_QIntervallResultCollection, gmlContext, GmlSerializer.DEFAULT_FACTORY );
    final QIntervallResultCollection resultCollection = (QIntervallResultCollection) workspace.getRootFeature();
    final Map<BigDecimal, QIntervallResult> pointResults = readProfFiles( resultCollection );

    if( m_log.checkCanceled() )
      return;

    readPolynomeFile( m_inputDir, pointResults, m_log );

    if( m_weirFilename != null )
      readBuildingFile( new File( m_inputDir, m_weirFilename ), pointResults, m_log );

    if( m_bridgeFilename != null )
      readBuildingFile( new File( m_inputDir, m_bridgeFilename ), pointResults, m_log );

    if( m_log.checkCanceled() )
      return;

    /* Write workspace into file */
    GmlSerializer.serializeWorkspace( m_targetGmlFile, workspace, "UTF-8" ); //$NON-NLS-1$
  }

  private Map<BigDecimal, QIntervallResult> readProfFiles( final QIntervallResultCollection resultCollection )
  {
    final Map<BigDecimal, QIntervallResult> results = new HashMap<BigDecimal, QIntervallResult>();

    /* Read w-points first: PROFxxx.xxxx.txt files */
    final FilenameFilter filter = new WildcardFileFilter( "PROF*.txt", IOCase.INSENSITIVE ); //$NON-NLS-1$ 
    final File[] profFiles = m_inputDir.listFiles( filter );
    if( profFiles == null || profFiles.length == 0 )
    {
      m_log.finish( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.17" ) ); //$NON-NLS-1$
      return results;
    }

    final SortedMap<BigDecimal, IProfileFeature> profileIndex = indexProfiles();

    for( final File profFile : profFiles )
    {
      final String name = profFile.getName();
      if( name.length() < 9 )
        continue;

      final String stationString = name.substring( 4, name.length() - 4 );
      final BigDecimal station = new BigDecimal( stationString );

      // REMARK: as the slope is not nicely written to the polynome result files, we get it from the calculation.
      // BUT: this is only valid for the REIB_KONST mode! So maybe we should change something later...?
      final BigDecimal startSlope = m_calculation.getStartSlope();
      final BigDecimal slope = startSlope.setScale( 5, RoundingMode.HALF_UP );

      try
      {
        final IFeatureBindingCollection<QIntervallResult> qIntervalls = resultCollection.getQIntervalls();
        final QIntervallResult qresult = qIntervalls.addNew( QIntervallResult.QNAME_F_QIntervallResult );
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
        readProfFile( profFile, observation.getResult(), m_log );
        qresult.setPointsObservation( observation );

        results.put( station, qresult );
      }
      catch( final Exception e )
      {
        m_log.log( e, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.20" ), name ); //$NON-NLS-1$
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

  private SortedMap<BigDecimal, IProfileFeature> indexProfiles( )
  {
    final TuhhReach reach = m_calculation.getReach();
    final TuhhReachProfileSegment[] reachProfileSegments = reach.getReachProfileSegments();
    final SortedMap<BigDecimal, IProfileFeature> index = new TreeMap<BigDecimal, IProfileFeature>();
    for( final TuhhReachProfileSegment segment : reachProfileSegments )
    {
      final IProfileFeature profileMember = segment.getProfileMember();
      index.put( segment.getStation(), profileMember );
    }

    return index;
  }

  private TupleResult readProfFile( final File profFile, final TupleResult tupleResult, final LogHelper log ) throws IOException
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

  private void readPolynomeFile( final File resultDir, final Map<BigDecimal, QIntervallResult> pointResults, final LogHelper log ) throws IOException
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
  private void readBuildingFile( final File buildingFile, final Map<BigDecimal, QIntervallResult> pointResults, final LogHelper log ) throws IOException
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

  private void readBuildingLine( final String line, final Map<BigDecimal, QIntervallResult> pointResults, final File buildingFile ) throws Exception
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
      /* Create a new Point Result if not yet existent */
      // REMARK: hopefully, there is always at least one result
      final QIntervallResult firstqResult = pointResults.values().iterator().next();
      final QIntervallResultCollection qresultCollection = (QIntervallResultCollection) firstqResult.getOwner();
      final IFeatureBindingCollection<QIntervallResult> qIntervalls = qresultCollection.getQIntervalls();
      final QIntervallResult newqresult = qIntervalls.addNew( QIntervallResult.QNAME_F_QIntervallResult );
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