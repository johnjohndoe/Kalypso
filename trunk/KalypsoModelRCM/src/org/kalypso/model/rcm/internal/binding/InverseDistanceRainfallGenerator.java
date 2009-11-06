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
package org.kalypso.model.rcm.internal.binding;

import java.math.BigInteger;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.rcm.binding.IOmbrometer;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.model.rcm.internal.UrlCatalogRcm;
import org.kalypso.model.rcm.util.RainfallGeneratorUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.transformation.GeoTransformer;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.FeaturePath;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

/**
 * Implementation that generates rainfall from ombrometer stations with inverse distance weighting.
 * 
 * @author Holger Albert
 */
public class InverseDistanceRainfallGenerator extends Feature_Impl implements IRainfallGenerator
{
  public static final QName QNAME = new QName( UrlCatalogRcm.NS_RCM, "InverseDistanceRainfallGenerator" );

  public static final QName QNAME_PROP_ombrometerCollection = new QName( UrlCatalogRcm.NS_RCM, "ombrometerCollection" );

  public static final QName QNAME_PROP_ombrometerFeaturePath = new QName( UrlCatalogRcm.NS_RCM, "ombrometerFeaturePath" );

  public static final QName QNAME_PROP_timeseriesLinkPath = new QName( UrlCatalogRcm.NS_RCM, "timeseriesLinkPath" );

  public static final QName QNAME_PROP_stationLocationPath = new QName( UrlCatalogRcm.NS_RCM, "stationLocationPath" );

  public static final QName QNAME_PROP_numberOmbrometers = new QName( UrlCatalogRcm.NS_RCM, "numberOmbrometers" );

  public static final QName QNAME_PROP_catchmentAreaPath = new QName( UrlCatalogRcm.NS_RCM, "catchmentAreaPath" );

  /**
   * The constructor.
   * 
   * @param parent
   *          The parent.
   * @param parentRelation
   *          The parent relation.
   * @param featureType
   *          The feature type.
   * @param id
   *          The feature id.
   * @param propValues
   *          The property values.
   */
  public InverseDistanceRainfallGenerator( Object parent, IRelationType parentRelation, IFeatureType featureType, String id, Object[] propValues )
  {
    super( parent, parentRelation, featureType, id, propValues );
  }

  /**
   * @see org.kalypso.model.rcm.binding.IRainfallGenerator#createRainfall(org.kalypsodeegree.model.feature.Feature[],
   *      java.util.Date, java.util.Date, org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IObservation[] createRainfall( Feature[] catchmentFeatures, Date from, Date to, IProgressMonitor monitor ) throws CoreException
  {
    /* Get the needed properties. */
    Feature ombrometerCollection = getProperty( QNAME_PROP_ombrometerCollection, Feature.class );
    String collectionPath = getProperty( QNAME_PROP_ombrometerFeaturePath, String.class );
    String linkPath = getProperty( QNAME_PROP_timeseriesLinkPath, String.class );
    String stationLocationPath = getProperty( QNAME_PROP_stationLocationPath, String.class );
    BigInteger numberOmbrometers = getProperty( QNAME_PROP_numberOmbrometers, BigInteger.class );
    String catchmentAreaPath = getProperty( QNAME_PROP_catchmentAreaPath, String.class );

    try
    {
      /* Get the ombrometers. */
      FeatureList ombrometerList = (FeatureList) new FeaturePath( collectionPath ).getFeatureForSegment( ombrometerCollection.getWorkspace(), ombrometerCollection, 0 );

      /* Convert to an array. */
      List<Feature> featureList = new ArrayList<Feature>( ombrometerList.size() );
      GMLWorkspace workspace = ombrometerList.getParentFeature().getWorkspace();
      for( Object object : ombrometerList )
      {
        Feature feature = FeatureHelper.getFeature( workspace, object );
        if( feature != null )
        {
          // TODO Should be in the generator gml (rcm) ...
          Boolean active = (Boolean) feature.getProperty( IOmbrometer.QNAME_PROP_ISUSED );
          if( active != null && active.booleanValue() == true )
            featureList.add( feature );
        }
      }

      /* Convert to an array. */
      Feature[] ombrometerFeatures = featureList.toArray( new Feature[featureList.size()] );

      /* Convert to zml observations . */
      TimeseriesLinkType[] ombrometerLinks = FeatureHelper.getProperties( ombrometerFeatures, linkPath, new TimeseriesLinkType[ombrometerFeatures.length] );
      URL sourceContext = ombrometerList.getParentFeature().getWorkspace().getContext();
      IObservation[] ombrometerObservations = RainfallGeneratorUtilities.readObservations( ombrometerLinks, from, to, sourceContext );

      /* Get the station locations. */
      GM_Point[] ombrometerStations = FeatureHelper.getProperties( ombrometerFeatures, stationLocationPath, new GM_Point[ombrometerFeatures.length] );

      /* Convert to JTS geometries. */
      Point[] ombrometerPoints = new Point[ombrometerStations.length];
      GeoTransformer transformer = new GeoTransformer( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
      for( int i = 0; i < ombrometerStations.length; i++ )
      {
        GM_Point ombrometerPoint = ombrometerStations[i];
        GM_Object ombrometerTransformed = transformer.transform( ombrometerPoint );
        ombrometerPoints[i] = (Point) JTSAdapter.export( ombrometerTransformed );
      }

      /* Get all catchment areas. */
      GM_MultiSurface[] areas = RainfallGeneratorUtilities.findCatchmentAreas( catchmentFeatures, catchmentAreaPath );

      /* Iterate through all catchments. */
      IObservation[] result = new IObservation[areas.length];
      for( int i = 0; i < areas.length; i++ )
      {
        /* Get the catchment. */
        GM_MultiSurface area = areas[i];
        if( area == null )
          continue;

        /* Convert to a JTS geometry. */
        Geometry areaGeometry = JTSAdapter.export( area );

        /* Get the weights. */
        double[] weights = getWeights( areaGeometry, ombrometerPoints, numberOmbrometers.intValue() );

        /* Combine the observations. */
        result[i] = RainfallGeneratorUtilities.combineObses( ombrometerObservations, weights );
      }

      return result;
    }
    catch( GM_Exception e )
    {
      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Failed to convert Geometrie: " + e.toString(), e ) );
    }
    catch( SensorException e )
    {
      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Failed to combine Observations: " + e.toString(), e ) );
    }
    catch( MalformedURLException e )
    {
      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Failed to load Observations: " + e.toString(), e ) );
    }
    catch( Exception e )
    {
      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Failed to create the rainfall: " + e.toString(), e ) );
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
  private double[] getWeights( Geometry areaGeometry, Point[] ombrometerPoints, int numberOmbrometers )
  {
    /* Get the inverse distance element, containing the factor, for each omrometer observation. */
    /* They will be still sorted by the distance to the area. */
    List<InverseDistanceElement> elements = getFactors( areaGeometry, ombrometerPoints, numberOmbrometers );

    /* So bring the factors in the right order again. */
    double[] weights = new double[ombrometerPoints.length];
    for( int i = 0; i < elements.size(); i++ )
    {
      /* Get the inverse distance element, containing the factor. */
      InverseDistanceElement element = elements.get( i );

      /* Get the index. */
      int index = element.getIndex();

      /* Get the factor. */
      double factor = element.getFactor();

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
  private List<InverseDistanceElement> getFactors( Geometry areaGeometry, Point[] ombrometerPoints, int numberOmbrometers )
  {
    /* Create the inverse distance elements. */
    List<InverseDistanceElement> elements = getInverseDistanceElemets( areaGeometry, ombrometerPoints );

    /* The sum of all distances. */
    double sumDistances = 0.0;

    /* Calculate the distances. */
    List<Double> distances = new ArrayList<Double>();
    for( int i = 0; i < elements.size(); i++ )
    {
      if( numberOmbrometers > 0 && i >= numberOmbrometers )
        break;

      /* Get the inverse distance element. */
      InverseDistanceElement element = elements.get( i );

      /* Get the distance. */
      double distance = 1 / element.getDistance();

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
      InverseDistanceElement element = elements.get( i );

      /* Get the distance. */
      Double distance = distances.get( i );

      /* Calculate the factor. */
      double factor = (distance.doubleValue() / sumDistances);

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
  private List<InverseDistanceElement> getInverseDistanceElemets( Geometry areaGeometry, Point[] ombrometerPoints )
  {
    /* Memory for the results. */
    List<InverseDistanceElement> results = new ArrayList<InverseDistanceElement>();

    for( int i = 0; i < ombrometerPoints.length; i++ )
    {
      /* Get the ombrometer point. */
      Point ombrometerPoint = ombrometerPoints[i];

      /* Create the inverse distance element. */
      InverseDistanceElement element = new InverseDistanceElement( areaGeometry, ombrometerPoint, i );

      /* Add to the results. */
      results.add( element );
    }

    /* Sort the results. */
    Collections.sort( results, null );

    return results;
  }
}