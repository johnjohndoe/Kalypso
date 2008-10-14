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
package org.kalypso.model.rcm.internal.binding;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Date;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.model.rcm.internal.UrlCatalogRcm;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.filters.NOperationFilter;
import org.kalypso.ogc.sensor.filter.filters.OperationFilter;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.FeaturePath;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Polygon;

/**
 * Implementation that generates rainfall from ombrometer stations.
 * 
 * @author Gernot Belger
 */
public class OmbrometerRainfallGenerator extends Feature_Impl implements IRainfallGenerator
{
  public static final QName QNAME = new QName( UrlCatalogRcm.NS_RCM, "OmbrometerRainfallGenerator" );

  public static final QName QNAME_PROP_ombrometerCollection = new QName( UrlCatalogRcm.NS_RCM, "ombrometerCollection" );

  public static final QName QNAME_PROP_ombrometerFeaturePath = new QName( UrlCatalogRcm.NS_RCM, "ombrometerFeaturePath" );

  public static final QName QNAME_PROP_timeseriesLinkPath = new QName( UrlCatalogRcm.NS_RCM, "timeseriesLinkPath" );

  public static final QName QNAME_PROP_areaPath = new QName( UrlCatalogRcm.NS_RCM, "areaPath" );


  public OmbrometerRainfallGenerator( final Object parent, final IRelationType parentRelation, final IFeatureType featureType, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, featureType, id, propValues );
  }

  /**
   * @see
   *      org.kalypso.model.rcm.binding.IRainfallGenerator#createRainfall(org.kalypsodeegree.model.geometry.GM_Surface<org
   *      .kalypsodeegree.model.geometry.GM_SurfacePatch>[], java.util.Date, java.util.Date,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  public IObservation[] createRainfall( final GM_Surface<GM_SurfacePatch>[] areas, final Date from, final Date to, final IProgressMonitor monitor ) throws org.eclipse.core.runtime.CoreException
  {
    final Feature ombrometerCollection = getProperty( QNAME_PROP_ombrometerCollection, Feature.class );
    final String collectionPath = getProperty( QNAME_PROP_ombrometerFeaturePath, String.class );
    final FeatureList ombrometerList = (FeatureList) new FeaturePath( collectionPath ).getFeatureForSegment( ombrometerCollection.getWorkspace(), ombrometerCollection, 0 );
    final String areaPath = getProperty( QNAME_PROP_areaPath, String.class );
    final String linkPath = getProperty( QNAME_PROP_timeseriesLinkPath, String.class );

    // Find the ombrometer-areas
    final Feature[] ombrometerFeatures = FeatureHelper.toArray( ombrometerList );
    if( ombrometerFeatures.length < ombrometerList.size() )
    {
      // TODO: log problem
    }

    final GM_Surface< ? >[] ombrometerAreas = FeatureHelper.getProperties( ombrometerFeatures, areaPath, new GM_Surface[ombrometerFeatures.length] );
    final TimeseriesLinkType[] ombrometerLinks = FeatureHelper.getProperties( ombrometerFeatures, linkPath, new TimeseriesLinkType[ombrometerFeatures.length] );
    final URL sourceContext = ombrometerList.getParentFeature().getWorkspace().getContext();

    try
    {
      final IObservation[] ombrometerObservations = readObservations( ombrometerLinks, from, to, sourceContext );

      // Convert to JTS-Geometries
      final Polygon[] ombrometerPolygons = new Polygon[ombrometerAreas.length];
      for( int i = 0; i < ombrometerAreas.length; i++ )
      {
        if( ombrometerAreas[i] != null )
          ombrometerPolygons[i] = (Polygon) JTSAdapter.export( ombrometerAreas[i] );
      }

      // Iterate through all catchments
      final IObservation[] result = new IObservation[areas.length];
      for( int i = 0; i < areas.length; i++ )
      {
        final GM_Surface<GM_SurfacePatch> area = areas[i];
        if( area == null )
          continue;

        final Polygon areaPolygon = (Polygon) JTSAdapter.export( area );
        final double[] weights = JTSUtilities.fractionAreasOf( areaPolygon, ombrometerPolygons );
        result[i] = combineObses( ombrometerObservations, weights );
      }

      return result;
    }
    catch( final GM_Exception e )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, "Failed to convert Geometrie: " + e.toString(), e );
      throw new CoreException( status );
    }
    catch( final SensorException e )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, "Failed to combine Observations: " + e.toString(), e );
      throw new CoreException( status );
    }
    catch( final MalformedURLException e )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, "Failed to load Observations: " + e.toString(), e );
      throw new CoreException( status );
    }
  }

  private IObservation[] readObservations( final TimeseriesLinkType[] ombrometerLinks, final Date from, final Date to, final URL context ) throws MalformedURLException, SensorException
  {
    final IRequest request = new ObservationRequest( from, to );

    final IObservation[] readObservations = new IObservation[ombrometerLinks.length];
    for( int i = 0; i < ombrometerLinks.length; i++ )
    {
      final TimeseriesLinkType link = ombrometerLinks[i];
      if( link != null )
      {
        final String href = link.getHref();
        if( href != null )
        {
          final String hrefRequest = ZmlURL.insertRequest( href, request );
          final URL zmlLocation = link == null ? null : UrlResolverSingleton.resolveUrl( context, hrefRequest );
          if( zmlLocation != null )
            readObservations[i] = ZmlFactory.parseXML( zmlLocation, href );
        }
      }
    }

    return readObservations;
  }

  private IObservation combineObses( final IObservation[] observations, final double[] weights ) throws SensorException
  {
    Assert.isTrue( observations.length == weights.length );

    final IObservation[] weightedObervations = new IObservation[observations.length];
    for( int i = 0; i < weights.length; i++ )
      weightedObervations[i] = new OperationFilter( OperationFilter.OPERATION_MAL, weights[i], observations[i] );

    return new NOperationFilter( NOperationFilter.OPERATION_PLUS, weightedObervations );
  }

}
