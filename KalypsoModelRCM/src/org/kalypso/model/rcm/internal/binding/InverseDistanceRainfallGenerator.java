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
package org.kalypso.model.rcm.internal.binding;

import java.math.BigInteger;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.rcm.binding.AbstractRainfallGenerator;
import org.kalypso.model.rcm.binding.IOmbrometer;
import org.kalypso.model.rcm.internal.KalypsoModelRcmActivator;
import org.kalypso.model.rcm.internal.UrlCatalogRcm;
import org.kalypso.model.rcm.util.RainfallGeneratorUtilities;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.transformation.transformer.GeoTransformerFactory;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypso.utils.log.LogUtilities;
import org.kalypso.zml.core.filter.binding.IZmlFilter;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

/**
 * Implementation that generates rainfall from ombrometer stations with inverse distance weighting.
 * 
 * @author Holger Albert
 */
public class InverseDistanceRainfallGenerator extends AbstractRainfallGenerator
{
  static final QName FEATURE_INVERSE_DISTANCE_RAINFALL_GENERATOR = new QName( UrlCatalogRcm.NS_RCM, "InverseDistanceRainfallGenerator" );

  static final QName MEMBER_ombrometerCollection = new QName( UrlCatalogRcm.NS_RCM, "ombrometerCollection" );

  static final QName PROPERTY_ombrometerFeaturePath = new QName( UrlCatalogRcm.NS_RCM, "ombrometerFeaturePath" );

  static final QName PROPERTY_timeseriesLinkPath = new QName( UrlCatalogRcm.NS_RCM, "timeseriesLinkPath" );

  static final QName MEMBER_FILTER = new QName( UrlCatalogRcm.NS_RCM, "filterMember" );

  static final QName PROPERTY_stationLocationPath = new QName( UrlCatalogRcm.NS_RCM, "stationLocationPath" );

  static final QName PROPERTY_numberOmbrometers = new QName( UrlCatalogRcm.NS_RCM, "numberOmbrometers" );

  static final QName PROPERTY_catchmentAreaPath = new QName( UrlCatalogRcm.NS_RCM, "catchmentAreaPath" );

  private final FeatureBindingCollection<IZmlFilter> m_filters = new FeatureBindingCollection<IZmlFilter>( this, IZmlFilter.class, MEMBER_FILTER, true );

  public InverseDistanceRainfallGenerator( final Object parent, final IRelationType parentRelation, final IFeatureType featureType, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, featureType, id, propValues );
  }

  @Override
  public IObservation[] createRainfall( final Feature[] catchmentFeatures, final DateRange range, final ILog log, IProgressMonitor monitor ) throws CoreException
  {
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    /* Monitor. */
    monitor.beginTask( "Führe Generator Ombrometer (Inverse Distanz) aus...", 1000 );
    monitor.subTask( "Prüfe Voraussetzungen..." );

    /* Update the log. */
    LogUtilities.logQuietly( log, new Status( IStatus.INFO, KalypsoModelRcmActivator.PLUGIN_ID, "Generator Ombrometer (Inverse Distanz) wurde gestartet.", null ) );

    /* Get the needed properties. */
    final Feature ombrometerCollection = getProperty( MEMBER_ombrometerCollection, Feature.class );
    final String collectionPath = getProperty( PROPERTY_ombrometerFeaturePath, String.class );
    final String linkPath = getProperty( PROPERTY_timeseriesLinkPath, String.class );
    final String stationLocationPath = getProperty( PROPERTY_stationLocationPath, String.class );
    final BigInteger numberOmbrometers = getProperty( PROPERTY_numberOmbrometers, BigInteger.class );
    final String catchmentAreaPath = getProperty( PROPERTY_catchmentAreaPath, String.class );

    /* Create the paths. */
    final GMLXPath collectionXPath = new GMLXPath( collectionPath, getWorkspace().getNamespaceContext() );
    final GMLXPath linkXPath = new GMLXPath( linkPath, getWorkspace().getNamespaceContext() );
    final GMLXPath stationLocationXPath = new GMLXPath( stationLocationPath, getWorkspace().getNamespaceContext() );
    final GMLXPath catchmentAreaXPath = new GMLXPath( catchmentAreaPath, getWorkspace().getNamespaceContext() );

    /* Monitor. */
    monitor.worked( 100 );
    monitor.subTask( "Lade Ombrometer..." );

    try
    {
      /* Get the ombrometers. */
      final FeatureList ombrometerList = (FeatureList) GMLXPathUtilities.query( collectionXPath, ombrometerCollection );

      /* Convert to an array. */
      final List<Feature> featureList = new ArrayList<Feature>( ombrometerList.size() );
      final GMLWorkspace workspace = ombrometerList.getParentFeature().getWorkspace();
      for( final Object object : ombrometerList )
      {
        final Feature feature = FeatureHelper.getFeature( workspace, object );
        if( feature != null )
        {
          // TODO Should be in the generator gml (rcm) ...
          final Boolean active = (Boolean) feature.getProperty( IOmbrometer.PROPERTY_ISUSED );
          if( active != null && active.booleanValue() == true )
            featureList.add( feature );
        }
      }

      /* Convert to an array. */
      final Feature[] ombrometerFeatures = featureList.toArray( new Feature[featureList.size()] );

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( "Konvertiere..." );

      /* Convert to zml observations . */
      final IZmlFilter[] filters = m_filters.toArray( new IZmlFilter[m_filters.size()] );

      final IObservation[] ombrometerObservations = RainfallGeneratorUtilities.readObservations( ombrometerFeatures, linkXPath, filters, range );

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( "Erzeuge Ombrometerpunkte..." );

      /* Get the station locations. */
      final GM_Point[] ombrometerStations = FeatureHelper.getProperties( ombrometerFeatures, stationLocationXPath, new GM_Point[ombrometerFeatures.length] );

      /* Convert to JTS geometries. */
      final Point[] ombrometerPoints = new Point[ombrometerStations.length];
      final IGeoTransformer transformer = GeoTransformerFactory.getGeoTransformer( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
      for( int i = 0; i < ombrometerStations.length; i++ )
      {
        final GM_Point ombrometerPoint = ombrometerStations[i];
        final GM_Object ombrometerTransformed = transformer.transform( ombrometerPoint );
        ombrometerPoints[i] = (Point) JTSAdapter.export( ombrometerTransformed );

        /* Monitor. */
        monitor.worked( 200 / ombrometerStations.length );
      }

      /* Monitor. */
      monitor.subTask( "Hole Einzugsgebiete..." );

      /* Get all catchment areas. */
      final GM_MultiSurface[] areas = RainfallGeneratorUtilities.findCatchmentAreas( catchmentFeatures, catchmentAreaXPath );

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( "Bearbeite Einzugsgebiete..." );

      /* Iterate through all catchments. */
      final IObservation[] result = new IObservation[areas.length];
      for( int i = 0; i < areas.length; i++ )
      {
        /* Monitor. */
        monitor.subTask( String.format( "Bearbeite Einzugsgebiet %d / %d...", i + 1, areas.length ) );

        /* Get the catchment. */
        final GM_MultiSurface area = areas[i];
        if( area == null )
        {
          monitor.worked( 400 / areas.length );
          continue;
        }

        /* Convert to a JTS geometry. */
        final Geometry areaGeometry = JTSAdapter.export( area );

        /* Get the weights. */
        final double[] weights = getWeights( areaGeometry, ombrometerPoints, numberOmbrometers.intValue() );

        /* Combine the observations. */
        result[i] = RainfallGeneratorUtilities.combineObses( ombrometerObservations, weights, "ombrometer://inverse.distance" );

        /* Monitor. */
        monitor.worked( 400 / areas.length );
      }

      /* Update the log. */
      LogUtilities.logQuietly( log, new Status( IStatus.OK, KalypsoModelRcmActivator.PLUGIN_ID, "Berechnet", null ) );

      return result;
    }
    catch( final GM_Exception e )
    {
      /* Update the log. */
      LogUtilities.logQuietly( log, new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, String.format( "Generator Ombrometer (Inverse Distanz) wurde mit einem Fehler beendet: %s", e.getLocalizedMessage() ), e ) );

      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Failed to convert Geometrie: " + e.toString(), e ) );
    }
    catch( final SensorException e )
    {
      /* Update the log. */
      LogUtilities.logQuietly( log, new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, String.format( "Generator Ombrometer (Inverse Distanz) wurde mit einem Fehler beendet: %s", e.getLocalizedMessage() ), e ) );

      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Failed to combine Observations: " + e.toString(), e ) );
    }
    catch( final MalformedURLException e )
    {
      /* Update the log. */
      LogUtilities.logQuietly( log, new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, String.format( "Generator Ombrometer (Inverse Distanz) wurde mit einem Fehler beendet: %s", e.getLocalizedMessage() ), e ) );

      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Failed to load Observations: " + e.toString(), e ) );
    }
    catch( final Exception e )
    {
      /* Update the log. */
      LogUtilities.logQuietly( log, new Status( IStatus.ERROR, KalypsoModelRcmActivator.PLUGIN_ID, String.format( "Generator Ombrometer (Inverse Distanz) wurde mit einem Fehler beendet: %s", e.getLocalizedMessage() ), e ) );

      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Failed to create the rainfall: " + e.toString(), e ) );
    }
    finally
    {
      /* Update the log. */
      LogUtilities.logQuietly( log, new Status( IStatus.INFO, KalypsoModelRcmActivator.PLUGIN_ID, "Generator Ombrometer (Inverse Distanz) wurde beendet.", null ) );

      /* Monitor. */
      monitor.done();
    }
  }

  /**
   * This function determines the weights (factors) for each ombrometer observation using the inverse distance
   * weighting.
   * 
   * @param areaGeometry
   *          The area geometry of the catchment.
   * @param ombrometerPoints
   *          The point geometries of the ombrometer stations (in the same order as the ombrometers).
   * @param numberOmbrometers
   *          The number of ombrometers which should be used in the inverse distance weighting. The nearest ones will be
   *          used. The unused ombrometers will get a 0.0 factor.
   * @return The weights (factors) for each ombrometer observation (in the same order as the ombrometers).
   */
  private double[] getWeights( final Geometry areaGeometry, final Point[] ombrometerPoints, final int numberOmbrometers )
  {
    /* Get the inverse distance element, containing the factor, for each omrometer observation. */
    /* They will be still sorted by the distance to the area. */
    final List<InverseDistanceElement> elements = getFactors( areaGeometry, ombrometerPoints, numberOmbrometers );

    /* So bring the factors in the right order again. */
    final double[] weights = new double[ombrometerPoints.length];
    for( int i = 0; i < elements.size(); i++ )
    {
      /* Get the inverse distance element, containing the factor. */
      final InverseDistanceElement element = elements.get( i );

      /* Get the index. */
      final int index = element.getIndex();

      /* Get the factor. */
      final double factor = element.getFactor();

      /* Set it at the right place. */
      weights[index] = factor;
    }

    return weights;
  }

  /**
   * This function determines the factors for each ombrometer observation using the inverse distance weighting.
   * 
   * @param areaGeometry
   *          The area geometry of the catchment.
   * @param ombrometerPoints
   *          The point geometries of the ombrometer stations (in the same order as the ombrometers).
   * @param numberOmbrometers
   *          The number of ombrometers which should be used in the inverse distance weighting. The nearest ones will be
   *          used. The unused ombrometers will get a 0.0 factor.
   * @return The factors for each ombrometer observation (they will be sorted by the distance to the area).
   */
  private List<InverseDistanceElement> getFactors( final Geometry areaGeometry, final Point[] ombrometerPoints, final int numberOmbrometers )
  {
    /* Create the inverse distance elements. */
    final List<InverseDistanceElement> elements = getInverseDistanceElemets( areaGeometry, ombrometerPoints );

    /* The sum of all distances. */
    double sumDistances = 0.0;

    /* Calculate the distances. */
    final List<Double> distances = new ArrayList<Double>();
    for( int i = 0; i < elements.size(); i++ )
    {
      if( numberOmbrometers > 0 && i >= numberOmbrometers )
        break;

      /* Get the inverse distance element. */
      final InverseDistanceElement element = elements.get( i );

      /* Get the distance. */
      final double distance = 1 / element.getDistance();

      /* First add it to the sum of distances. */
      sumDistances = sumDistances + distance;

      /* Then add it to the list of distances. */
      distances.add( new Double( distance ) );
    }

    /* Calculate the factors. */
    for( int i = 0; i < elements.size(); i++ )
    {
      if( numberOmbrometers > 0 && i >= numberOmbrometers )
        break;

      /* Get the inverse distance element. */
      final InverseDistanceElement element = elements.get( i );

      /* Get the distance. */
      final Double distance = distances.get( i );

      /* Calculate the factor. */
      final double factor = (distance.doubleValue() / sumDistances);

      /* Add it to the corresponding element. */
      element.setFactor( factor );
    }

    return elements;
  }

  /**
   * This function returns a list of inverse distance elements. The first item of an element will be always the
   * parameter area geometry and the second item of an element will be a ombrometer point of the list. The elements will
   * be sorted by the distance of the contained items.
   * 
   * @param areaGeometry
   *          The area geometry of the catchment.
   * @param ombrometerPoints
   *          The point geometries of the ombrometer stations (in the same order as the ombrometers).
   * @return A list of inverse distance elements. The first item of an element will be always the parameter area
   *         geometry and the second item of an element will be a ombrometer point of the list. The elements will be
   *         sorted by the distance of the contained items.
   */
  private List<InverseDistanceElement> getInverseDistanceElemets( final Geometry areaGeometry, final Point[] ombrometerPoints )
  {
    /* Memory for the results. */
    final List<InverseDistanceElement> results = new ArrayList<InverseDistanceElement>();

    for( int i = 0; i < ombrometerPoints.length; i++ )
    {
      /* Get the ombrometer point. */
      final Point ombrometerPoint = ombrometerPoints[i];

      /* Create the inverse distance element. */
      final InverseDistanceElement element = new InverseDistanceElement( areaGeometry, ombrometerPoint, i );

      /* Add to the results. */
      results.add( element );
    }

    /* Sort the results. */
    Collections.sort( results, null );

    return results;
  }
}