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
package org.kalypso.model.flood.util;

import java.util.LinkedList;
import java.util.List;

import org.kalypso.model.flood.binding.IFloodPolygon;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Thomas Jung
 *
 */
public class FloodPolygonHelper
{
  /**
   * returns the polygons that are valid for a given event
   *
   * @param event
   *            the event for which we are searching in the polygons
   * @param polygons
   *            the polygons that are examined
   *
   * @return an array of {@link IFloodPolygon}, that is valid for the given event
   *
   */
  public static IFloodPolygon[] getPolygonsForEvent( final IRunoffEvent event, final IFeatureBindingCollection<IFloodPolygon> polygons )
  {
    /* collect all polygons, that are valid for the selected event */
    final List<IFloodPolygon> polygonList = new LinkedList<>();

    // check each polygon
    for( final IFloodPolygon floodPolygon : polygons )
    {
      // get the feature
      final Feature polygonFeature = floodPolygon;

      // get the event, the polygon is valid for
      final IRunoffEvent runoffEvent = (IRunoffEvent) polygonFeature.getProperty( IFloodPolygon.QNAME_PROP_EVENT );

      // check
      if( runoffEvent.equals( event ) )
      {
        // add the polygon to a list
        polygonList.add( floodPolygon );
      }
    }
    return polygonList.toArray( new IFloodPolygon[polygonList.size()] );
  }

  /**
   * returns {@link SortedFloodPolygonMap} that are valid for a given event
   *
   * @param event
   *            the event for which we are searching in the polygons
   * @param polygons
   *            the polygons that are examined
   *
   * @return {@link SortedFloodPolygonMap}
   *
   */
  public static SortedFloodPolygonMap getSortedPolygonsForEvent( final IRunoffEvent event, final IFeatureBindingCollection<IFloodPolygon> polygons )
  {
    final IFloodPolygon[] polygonsForEvent = getPolygonsForEvent( event, polygons );

    final SortedFloodPolygonMap map = new SortedFloodPolygonMap();

    // fill the map
    for( final IFloodPolygon floodPolygon : polygonsForEvent )
    {
      map.add( floodPolygon );
    }

    return map;
  }

  /**
   * returns {@link SortedFloodPolygonMap} for a given polygon array
   *
   * @param polygons
   *            the polygons that are examined
   *
   * @return {@link SortedFloodPolygonMap}
   */
  public static SortedFloodPolygonMap getSortedPolygons( final IFeatureBindingCollection<IFloodPolygon> polygons )
  {
    final SortedFloodPolygonMap map = new SortedFloodPolygonMap();

    // fill the map
    for( final IFloodPolygon floodPolygon : polygons )
    {
      map.add( floodPolygon );
    }
    return map;
  }
}