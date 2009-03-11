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
package org.kalypso.jts;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.TopologyException;

/**
 * Utility class for snapping a point to geometrys.
 *
 * @author Holger Albert, Dirk Kuch
 */
public class SnapUtilities
{
  public static enum SNAP_TYPE
  {
    /**
     * Snaps the GM_Point to the next possible point.
     */
    SNAP_TO_POINT,
    /**
     * Snaps the GM_Point to the line, if points are near, one of this points will be used.
     */
    SNAP_AUTO,
    /**
     * Only snaps to the line, ignoring all points.
     */
    SNAP_TO_LINE;
  }

  private SnapUtilities( )
  {
  }

  /**
   * Returns an Point representing the point.
   */
  public static Point snapPoint( final Point pointJTS )
  {
    final GeometryFactory factory = new GeometryFactory( pointJTS.getPrecisionModel(), pointJTS.getSRID() );
    return factory.createPoint( pointJTS.getCoordinate() );
  }

  /**
   * Returns an Point snapped to the line.
   */
  public static Point snapLine( final LineString geometryJTS, final Geometry pointBuffer, final SNAP_TYPE type )
  {
    try
    {
      if( type.equals( SNAP_TYPE.SNAP_TO_POINT ) )
      {
        final Point point = JTSUtilities.linePointInGeometry( geometryJTS, pointBuffer );

        if( point != null )
          return point;

        return null;
      }
      else if( type.equals( SNAP_TYPE.SNAP_AUTO ) )
      {
        final Point point = snapLine( geometryJTS, pointBuffer, SNAP_TYPE.SNAP_TO_POINT );

        if( point != null )
          return point;

        return snapLine( geometryJTS, pointBuffer, SNAP_TYPE.SNAP_TO_LINE );
      }
      else if( type.equals( SNAP_TYPE.SNAP_TO_LINE ) )
      {
        final Geometry geometryIntersection = pointBuffer.intersection( geometryJTS );

        if( !(geometryIntersection instanceof LineString) )
          return null;

        final Point point = JTSUtilities.pointOnLinePercent( (LineString) geometryIntersection, 50 );

        if( point == null )
          return null;

        return point;
      }
    }
    catch( final TopologyException e )
    {
      e.printStackTrace();
    }

    return null;
  }

  /**
   * Returns an Point snapped to the polygon at the outside.
   */
  public static Point snapPolygon( final Polygon geometryJTS, final Geometry pointBuffer, final SNAP_TYPE type )
  {
    final LineString exteriorRing = geometryJTS.getExteriorRing();

    if( type.equals( SNAP_TYPE.SNAP_TO_POINT ) )
    {
      final Point point = JTSUtilities.linePointInGeometry( exteriorRing, pointBuffer );

      if( point != null )
        return point;

      return null;
    }
    else if( type.equals( SNAP_TYPE.SNAP_AUTO ) )
    {
      final Point point = snapPolygon( geometryJTS, pointBuffer, SNAP_TYPE.SNAP_TO_POINT );

      if( point != null )
        return point;

      return snapPolygon( geometryJTS, pointBuffer, SNAP_TYPE.SNAP_TO_LINE );
    }
    else if( type.equals( SNAP_TYPE.SNAP_TO_LINE ) )
    {
      final Geometry geometryIntersection = pointBuffer.intersection( exteriorRing );

      if( !(geometryIntersection instanceof LineString) )
        return null;

      final com.vividsolutions.jts.geom.Point point = JTSUtilities.pointOnLinePercent( (LineString) geometryIntersection, 50 );

      if( point == null )
        return null;

      return point;
    }

    return null;
  }
}