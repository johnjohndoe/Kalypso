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
  public static Point snapPoint( Point pointJTS )
  {
    GeometryFactory factory = new GeometryFactory( pointJTS.getPrecisionModel(), pointJTS.getSRID() );
    return factory.createPoint( pointJTS.getCoordinate() );
  }

  /**
   * Returns an Point snapped to the line.
   */
  public static Point snapLine( LineString geometryJTS, Geometry pointBuffer, SNAP_TYPE type )
  {
    if( type.equals( SNAP_TYPE.SNAP_TO_POINT ) )
    {
      Point point = JTSUtilities.linePointInGeometry( geometryJTS, pointBuffer );

      if( point != null )
        return point;

      return null;
    }
    else if( type.equals( SNAP_TYPE.SNAP_AUTO ) )
    {
      Point point = snapLine( geometryJTS, pointBuffer, SNAP_TYPE.SNAP_TO_POINT );

      if( point != null )
        return point;

      return snapLine( geometryJTS, pointBuffer, SNAP_TYPE.SNAP_TO_LINE );
    }
    else if( type.equals( SNAP_TYPE.SNAP_TO_LINE ) )
    {
      Geometry geometryIntersection = pointBuffer.intersection( geometryJTS );

      if( !(geometryIntersection instanceof LineString) )
        return null;

      Point point = JTSUtilities.pointOnLinePercent( (LineString) geometryIntersection, 50 );

      if( point == null )
        return null;

      return point;
    }

    return null;
  }

  /**
   * Returns an Point snapped to the polygon at the outside.
   */
  public static Point snapPolygon( Polygon geometryJTS, Geometry pointBuffer, SNAP_TYPE type )
  {
    LineString exteriorRing = geometryJTS.getExteriorRing();

    if( type.equals( SNAP_TYPE.SNAP_TO_POINT ) )
    {
      Point point = JTSUtilities.linePointInGeometry( exteriorRing, pointBuffer );

      if( point != null )
        return point;

      return null;
    }
    else if( type.equals( SNAP_TYPE.SNAP_AUTO ) )
    {
      Point point = snapPolygon( geometryJTS, pointBuffer, SNAP_TYPE.SNAP_TO_POINT );

      if( point != null )
        return point;

      return snapPolygon( geometryJTS, pointBuffer, SNAP_TYPE.SNAP_TO_LINE );
    }
    else if( type.equals( SNAP_TYPE.SNAP_TO_LINE ) )
    {
      Geometry geometryIntersection = pointBuffer.intersection( exteriorRing );

      if( !(geometryIntersection instanceof LineString) )
        return null;

      com.vividsolutions.jts.geom.Point point = JTSUtilities.pointOnLinePercent( (LineString) geometryIntersection, 50 );

      if( point == null )
        return null;

      return point;
    }

    return null;
  }
}