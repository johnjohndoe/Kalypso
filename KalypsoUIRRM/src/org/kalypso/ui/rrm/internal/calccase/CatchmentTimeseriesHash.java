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
package org.kalypso.ui.rrm.internal.calccase;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.model.rcm.RainfallGenerationOp;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.obslink.TimeseriesLinkType;

/**
 * This class contains a hash with catchment->timeseries link. One catchment may have more than one timeseries link.
 * 
 * @author Holger Albert
 */
public class CatchmentTimeseriesHash
{
  /**
   * Hash catchment->timeseries link. One catchment may have more than one timeseries link.
   */
  private final Map<String, List<TimeseriesLinkType>> m_hash;

  /**
   * The constructor.
   */
  public CatchmentTimeseriesHash( )
  {
    m_hash = new HashMap<String, List<TimeseriesLinkType>>();
  }

  /**
   * This function adds the timeseries link to the given catchment.
   * 
   * @param catchmentId
   *          The id of the catchment.
   * @param link
   *          The timeseries link.
   */
  public void put( final String catchmentId, final TimeseriesLinkType link )
  {
    if( !m_hash.containsKey( catchmentId ) )
      m_hash.put( catchmentId, new ArrayList<TimeseriesLinkType>() );

    final List<TimeseriesLinkType> links = m_hash.get( catchmentId );
    links.add( link );
  }

  /**
   * This function returns a map, which contains ONE observation for each catchment id. This single observation is the
   * result of merging the whole list of observations (available in the hash) for that catchment.
   * 
   * @param context
   *          The context for loading the timeseries links.
   * @return A map with all ids of the catchment and their observations.
   */
  public Map<String, IObservation> merge( final URL context ) throws CoreException, MalformedURLException, SensorException
  {
    /* Memory for the results. */
    final HashMap<String, IObservation> results = new HashMap<String, IObservation>();

    /* Get the ids of the catchments. */
    final String[] keys = m_hash.keySet().toArray( new String[] {} );
    for( final String key : keys )
    {
      /* Get the timeseries links for the catchment. */
      final List<TimeseriesLinkType> timeseriesLinks = m_hash.get( key );

      /* Load them all. */
      final List<IObservation> observations = loadObservations( context, timeseriesLinks );

      /* Merge them. */
      final IObservation observation = RainfallGenerationOp.combineObservations( observations );

      /* Store the result. */
      results.put( key, observation );
    }

    return results;
  }

  /**
   * This function loads the observations.
   * 
   * @param context
   *          The context for loading the timeseries links.
   * @param timeseriesLinks
   *          The timeseries links.
   * @return A list of observations in the same order then the list of timeseries links.
   */
  private List<IObservation> loadObservations( final URL context, final List<TimeseriesLinkType> timeseriesLinks ) throws MalformedURLException, SensorException
  {
    /* Memory for the results. */
    final List<IObservation> results = new ArrayList<>();

    /* Load all links. */
    for( final TimeseriesLinkType link : timeseriesLinks )
    {
      /* The hrefs of observation. */
      final String href = link.getHref();

      /* Get the source observation. */
      final URL url = UrlResolverSingleton.resolveUrl( context, href );
      final IObservation observation = ZmlFactory.parseXML( url );

      /* Store the result. */
      results.add( observation );
    }

    return results;
  }
}