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
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ogc.sensor.zml.ZmlURL;
import org.kalypso.zml.obslink.TimeseriesLinkType;

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

  public static IObservation[] readObservations( TimeseriesLinkType[] ombrometerLinks, Date from, Date to, URL context ) throws MalformedURLException, SensorException
  {
    IRequest request = new ObservationRequest( from, to );

    IObservation[] readObservations = new IObservation[ombrometerLinks.length];
    for( int i = 0; i < ombrometerLinks.length; i++ )
    {
      TimeseriesLinkType link = ombrometerLinks[i];
      if( link != null )
      {
        String href = link.getHref();
        if( href != null )
        {
          String hrefRequest = ZmlURL.insertRequest( href, request );
          URL zmlLocation = link == null ? null : UrlResolverSingleton.resolveUrl( context, hrefRequest );
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
  public static IObservation combineObses( IObservation[] observations, double[] weights ) throws SensorException
  {
    /* There should be a weight for each observation. */
    Assert.isTrue( observations.length == weights.length );

    /* Here the array of the observations and the heights should have the same length. */
    /* So it is enough to check one of them. */
    if( observations.length == 0 )
      return null;

    /* Some things of the first observation. */
    IObservation firstObservation = observations[0];
    ITuppleModel firstTuppleModel = firstObservation.getValues( null );
    IAxis[] firstAxisList = firstTuppleModel.getAxisList();
    IAxis firstDateAxis = ObservationUtilities.findAxisByClass( firstAxisList, Date.class );

    /* Add the observation values and the rainfall axes in the same order as the observations are. */
    List<ITuppleModel> observationValues = new ArrayList<ITuppleModel>();
    List<IAxis> rainfallAxes = new ArrayList<IAxis>();
    for( int j = 0; j < observations.length; j++ )
    {
      /* Get the observation. */
      IObservation observation = observations[j];

      /* Get the values. */
      ITuppleModel values = observation.getValues( null );
      if( values.getCount() != firstTuppleModel.getCount() )
        throw new SensorException( "The observations in the list must have a equal number of elements ..." );

      /* Add. */
      observationValues.add( values );

      /* Get the axis list. */
      IAxis[] axisList = observation.getAxisList();

      /* Get the rainfall axis. */
      IAxis rainfallAxis = ObservationUtilities.findAxisByClass( axisList, Double.class );

      /* Add. */
      rainfallAxes.add( rainfallAxis );
    }

    /* Create a new observation using the axis of the first observation. */
    /* The other observations should have the same type of axes. */
    SimpleTuppleModel combinedTuppleModel = new SimpleTuppleModel( firstAxisList );
    SimpleObservation combinedObservation = new SimpleObservation( "", "", "", false, new MetadataList(), firstAxisList, combinedTuppleModel );
    IAxis[] combinedAxisList = combinedTuppleModel.getAxisList();
    IAxis combinedDateAxis = ObservationUtilities.findAxisByClass( combinedAxisList, Date.class );
    int combinedDatePosition = combinedTuppleModel.getPositionFor( combinedDateAxis );
    IAxis combinedDoubleAxis = ObservationUtilities.findAxisByClass( combinedAxisList, Double.class );
    int combinedDoublePosition = combinedTuppleModel.getPositionFor( combinedDoubleAxis );
    IAxis combinedIntegerAxis = ObservationUtilities.findAxisByClass( combinedAxisList, Integer.class );
    int combinedIntegerPosition = combinedTuppleModel.getPositionFor( combinedIntegerAxis );

    for( int i = 0; i < firstTuppleModel.getCount(); i++ )
    {
      double sum = 0.0;
      for( int j = 0; j < observations.length; j++ )
      {
        /* Get the weight. */
        double weight = weights[j];
        if( weight == 0.0 )
          continue;

        /* Get the values of the observation. */
        ITuppleModel tuppleModel = observationValues.get( j );

        /* Get the rainfall axis. */
        /* The date will be taken later from the first observation. */
        /* The status bit will be set to ok later. */
        IAxis rainfallAxis = rainfallAxes.get( j );

        /* Multiply the values of the current observation. */
        Double value = (Double) tuppleModel.getElement( i, rainfallAxis );

        /* Weight the value. */
        double weightedValue = value.doubleValue() * weight;

        /* Add to the sum. */
        sum = sum + weightedValue;
      }

      /* The list for the values. */
      Object[] values = new Object[3];

      /* Add the first date to the new observation. */
      Date firstDate = (Date) firstTuppleModel.getElement( i, firstDateAxis );
      values[combinedDatePosition] = new Date( firstDate.getTime() );

      /* Add the summarized values to the new observation. */
      values[combinedDoublePosition] = new Double( sum );

      /* Add the status bit to the new observation. */
      values[combinedIntegerPosition] = new Integer( KalypsoStati.BIT_OK );

      /* Add a new row. */
      combinedTuppleModel.addTupple( values );
    }

    return combinedObservation;
  }
}