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
package org.kalypso.model.rcm.util;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.TuppleModesLinearAdd;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathException;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Utilities for {@link org.kalypso.model.rcm.internal.binding.OmbrometerRainfallGenerator}'s and
 * {@link org.kalypso.model.rcm.internal.binding.InverseDistanceRainfallGenerator}'s.
 * 
 * @author Holger Albert
 */
public class RainfallGeneratorUtilities
{
  /**
   * The constructor.
   */
  private RainfallGeneratorUtilities( )
  {
  }

  @SuppressWarnings("unchecked")
  public static GM_MultiSurface[] findCatchmentAreas( final Feature[] catchmentFeatures, final GMLXPath catchmentAreaXPath ) throws CoreException, GMLXPathException
  {
    /* Memory for the results. */
    final GM_MultiSurface[] areas = new GM_MultiSurface[catchmentFeatures.length];

    for( int i = 0; i < catchmentFeatures.length; i++ )
    {
      final Feature catchmentFeature = catchmentFeatures[i];
      if( catchmentFeature == null )
        throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Ein catchment feature war null ...", null ) );

      final Object object = GMLXPathUtilities.query( catchmentAreaXPath, catchmentFeature );
      if( object instanceof GM_Surface )
      {
        final GM_Surface surface = (GM_Surface) object;
        final GM_MultiSurface multiSurface = GeometryFactory.createGM_MultiSurface( new GM_Surface[] { surface }, surface.getCoordinateSystem() );
        areas[i] = multiSurface;
      }
      else if( object instanceof GM_MultiSurface )
        areas[i] = (GM_MultiSurface) object;
      else if( object == null )
        catchmentFeatures[i] = null; // does not make sense to process
      else
        throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, String.format( "Ung¸ltiges Object in Zeitreihenlink: %s (Property: %s). Erwartet wird ein GM_Surface oder ein GM_MultiSurface.", object, catchmentAreaXPath ), null ) );
    }

    return areas;
  }

  public static IObservation[] readObservations( final TimeseriesLinkType[] ombrometerLinks, final Date from, final Date to, final URL context ) throws MalformedURLException, SensorException
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

  /**
   * This function combines the observations using the specified weights.
   * 
   * @param observations
   *          The observations to combine.
   * @param weights
   *          The weights to use.
   * @return A new combined observation.
   */
  public static IObservation combineObses( final IObservation[] observations, final double[] weights ) throws SensorException
  {
    /* There should be a weight for each observation. */
    Assert.isTrue( observations.length == weights.length );

    /* Here the array of the observations and the heights should have the same length. */
    /* So it is enough to check one of them. */
    if( observations.length == 0 )
      return null;

    /* Some things of the first observation. */
    final IObservation firstObservation = observations[0];
    final MetadataList firstMetadataList = firstObservation.getMetadataList();
    final String firstStart = firstMetadataList.getProperty( TimeserieUtils.MD_VORHERSAGE_START );
    final String firstEnde = firstMetadataList.getProperty( TimeserieUtils.MD_VORHERSAGE_ENDE );

    final List<ITuppleModel> observationValues = new ArrayList<ITuppleModel>();
    for( final IObservation observation : observations )
      observationValues.add( observation.getValues( null ) );
    final ITuppleModel[] tuppleModels = observationValues.toArray( new ITuppleModel[observationValues.size()] );

    final ITuppleModel firstTuppleModel = firstObservation.getValues( null );
    final IAxis[] firstAxisList = firstTuppleModel.getAxisList();

    final IAxis firstDateAxis = ObservationUtilities.findAxisByClass( firstAxisList, Date.class );
    final IAxis firstValueAxis = ObservationUtilities.findAxisByClass( firstAxisList, Double.class );
    final IAxis firstStatusAxis = ObservationUtilities.findAxisByClass( firstAxisList, Integer.class );

    final TuppleModesLinearAdd linearAdd = new TuppleModesLinearAdd( firstValueAxis.getType(), firstDateAxis, firstValueAxis, firstStatusAxis );
    final ITuppleModel combinedTuppleModel = linearAdd.addWeighted( tuppleModels, weights );

    final SimpleObservation combinedObservation = new SimpleObservation( "", "", "", false, new MetadataList(), firstAxisList, combinedTuppleModel );
    combinedObservation.setName( "Generierte Zeitreihe" );
    if( firstStart != null )
      combinedObservation.getMetadataList().setProperty( TimeserieUtils.MD_VORHERSAGE_START, firstStart );
    if( firstEnde != null )
      combinedObservation.getMetadataList().setProperty( TimeserieUtils.MD_VORHERSAGE_ENDE, firstEnde );

    return combinedObservation;
  }
}