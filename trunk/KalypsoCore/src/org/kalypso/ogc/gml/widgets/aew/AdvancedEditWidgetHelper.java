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

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.NotImplementedException;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * @author Dirk Kuch
 */
public class AdvancedEditWidgetHelper
{
  private static double PRECISION = 0.00001;

  public enum DIRECTION
  {
    eForward,
    eBackward;
  }

  /**
   * @param direction
   *          sometime a ring contains the same point twice or more
   */
  public static int getIndexOfPoint( final LineString ring, final Point point, final DIRECTION direction )
  {
    if( DIRECTION.eForward.equals( direction ) )
    {
      for( int i = 0; i < ring.getNumPoints(); i++ )
      {
        final Point p = ring.getPointN( i );
        if( p.distance( point ) < PRECISION )
          return i;
      }

      return -1;
    }
    else if( DIRECTION.eBackward.equals( direction ) )
    {
      for( int i = ring.getNumPoints() - 1; i >= 0; i-- )
      {
        final Point p = ring.getPointN( i );
        if( p.distance( point ) < PRECISION )
          return i;
      }

      return -1;
    }
    else
      throw new NotImplementedException();
  }

  public static int resolveNeighbor( final LineString ring, final Point current, final int index, final DIRECTION direction )
  {
    if( DIRECTION.eForward.equals( direction ) )
    {
      if( index < ring.getNumPoints() - 1 )
      {
        final Point p = ring.getPointN( index + 1 );
        if( p.distance( current ) < PRECISION )
          return resolveNeighbor( ring, current, index + 1, direction );
        else
          return index + 1;

      }
      else
        return resolveNeighbor( ring, current, 0, direction );
    }
    else if( DIRECTION.eBackward.equals( direction ) )
    {
      if( index > 0 )
      {
        final Point p = ring.getPointN( index - 1 );
        if( p.distance( current ) < PRECISION )
          return resolveNeighbor( ring, current, index - 1, direction );
        else
          return index - 1;
      }
      else
        return resolveNeighbor( ring, current, ring.getNumPoints() - 1, direction );
    }

    throw new NotImplementedException();
  }

  public static Polygon resolveResultPolygon( final Polygon polygon, final int indexCurrent, final Point moved )
  {
    final GeometryFactory factory = new GeometryFactory( polygon.getPrecisionModel(), polygon.getSRID() );
    final List<Coordinate> myCoordinates = new ArrayList<Coordinate>();

    final LineString ring = polygon.getExteriorRing();

    for( int i = 0; i < ring.getCoordinates().length; i++ )
    {
      if( i == indexCurrent )
      {
        myCoordinates.add( moved.getCoordinate() );
      }
      else if( i == ring.getNumPoints() - 1 )
      {
        myCoordinates.add( myCoordinates.get( 0 ) );
      }
      else
      {
        myCoordinates.add( ring.getCoordinateN( i ) );
      }
    }

    final LinearRing resultRing = factory.createLinearRing( myCoordinates.toArray( new Coordinate[] {} ) );
    return factory.createPolygon( resultRing, new LinearRing[] {} );
  }

  public static Polygon resolveResultPolygon( final Polygon polygon, final int indexPrevious, final int indexNext, final Point vector )
  {
    final GeometryFactory factory = new GeometryFactory( polygon.getPrecisionModel(), polygon.getSRID() );
    final List<Coordinate> myCoordinates = new ArrayList<Coordinate>();

    final LineString ring = polygon.getExteriorRing();

    if( indexPrevious < indexNext )
    {
      for( int i = 0; i < ring.getNumPoints(); i++ )
      {
        if( i > indexPrevious && i < indexNext )
        {
          final Coordinate c = ring.getCoordinateN( i );
          final Coordinate moved = new Coordinate( c.x - vector.getX(), c.y - vector.getY() );

          myCoordinates.add( moved );
        }
        else if( indexPrevious == 0 && i == ring.getNumPoints() - 1 )
        {
          myCoordinates.add( myCoordinates.get( 0 ) );
        }
        else
        {
          myCoordinates.add( ring.getCoordinateN( i ) );
        }
      }
    }
    else
    {
      for( int i = 0; i < ring.getNumPoints(); i++ )
      {
        if( i > indexPrevious || i < indexNext )
        {
          final Coordinate c = ring.getCoordinateN( i );
          final Coordinate moved = new Coordinate( c.x - vector.getX(), c.y - vector.getY() );

          myCoordinates.add( moved );
        }
        else if( i == ring.getNumPoints() - 1 )
        {
          myCoordinates.add( myCoordinates.get( 0 ) );
        }
        else
        {
          myCoordinates.add( ring.getCoordinateN( i ) );
        }
      }
    }

    final LinearRing resultRing = factory.createLinearRing( myCoordinates.toArray( new Coordinate[] {} ) );
    return factory.createPolygon( resultRing, new LinearRing[] {} );
  }
}
