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
package org.kalypso.ogc.gml.widgets.aew;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.apache.commons.lang.NotImplementedException;
import org.kalypsodeegree.model.feature.Feature;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * @author Dirk Kuch
 */
public class AdvancedEditWidgetSnapper
{

  public static IAdvancedEditWidgetSnappedPoint[] findSnapPoints( final Map<Geometry, Feature> geometries, final Point base, final double range )
  {
    final Set<IAdvancedEditWidgetSnappedPoint> myPoints = new LinkedHashSet<IAdvancedEditWidgetSnappedPoint>();

    final Set<Entry<Geometry, Feature>> entries = geometries.entrySet();
    for( final Entry<Geometry, Feature> entry : entries )
    {
      final Geometry geometry = entry.getKey();
      if( geometry instanceof Polygon )
      {
        final Polygon polygon = (Polygon) geometry;
        final LineString ring = polygon.getExteriorRing();

        // ignore last point - in a linear ring first and last point are always the same!
        for( int i = 0; i < ring.getNumPoints() - 1; i++ )
        {
          final Point point = ring.getPointN( i );

          if( point.distance( base ) <= range )
          {
            myPoints.add( new IAdvancedEditWidgetSnappedPoint()
            {
              @Override
              public Feature getFeature( )
              {
                return entry.getValue();
              }

              @Override
              public Point getMovedPoint( final Point vector )
              {
                final Point p = getPoint();
                final Coordinate c = new Coordinate( p.getX() - vector.getX(), p.getY() - vector.getY() );
                final GeometryFactory factory = new GeometryFactory( p.getPrecisionModel(), p.getSRID() );

                return factory.createPoint( c );
              }

              @Override
              public Point getPoint( )
              {
                return point;
              }
            } );
          }
        }

      }
      else
        throw new NotImplementedException();
    }

    return myPoints.toArray( new IAdvancedEditWidgetSnappedPoint[] {} );
  }

}
