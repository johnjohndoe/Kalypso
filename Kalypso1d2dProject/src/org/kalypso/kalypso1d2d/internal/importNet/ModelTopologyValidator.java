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
package org.kalypso.kalypso1d2d.internal.importNet;

import com.bce.gis.io.zweidm.IPolygonWithName;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.TopologyException;
import com.vividsolutions.jts.operation.valid.IsValidOp;
import com.vividsolutions.jts.operation.valid.TopologyValidationError;

/**
 * @author Gernot Belger
 */
public class ModelTopologyValidator
{
  public String validate( final IPolygonWithName item1, final IPolygonWithName item2 )
  {
    final Polygon polygon1 = item1.getPolygon();
    final Polygon polygon2 = item2.getPolygon();

    if( !polygon1.intersects( polygon2 ) )
      return null;

    try
    {
      final Geometry intersection = polygon1.intersection( polygon2 );
      return checkIntersection( intersection, item1, item2 );
    }
    catch( final TopologyException e )
    {
      final TopologyValidationError error = new IsValidOp( polygon1 ).getValidationError();
      if( error != null )
        return error.getMessage();

      // Ignore other geometry -> will eventually be validated again
      return null;
    }
  }

  private String checkIntersection( final Geometry intersection, final IPolygonWithName item1, final IPolygonWithName item2 )
  {
    if( intersection instanceof GeometryCollection )
      return "complex intersection";

    switch( intersection.getDimension() )
    {
      case 0:
        return check0dim( intersection, item1, item2 );

      case 1:
        return check1dim( intersection, item1, item2 );

      case 2:
        // never legal
        return String.format( "overlapping (area = %.4f)", intersection.getArea() );

      default:
        throw new IllegalStateException();
    }
  }

  private String check0dim( final Geometry intersection, final IPolygonWithName item1, final IPolygonWithName item2 )
  {
    if( !(intersection instanceof Point) )
      throw new IllegalStateException();

    final Point vertice = (Point) intersection;

    // 2 points, edge must be a real edge of both polygons
    if( !isVerticeOf( item1.getPolygon(), vertice.getCoordinate() ) )
      return String.format( "intersection is not a vertice of item '%s'", item1.getName() );

    if( !isVerticeOf( item2.getPolygon(), vertice.getCoordinate() ) )
      return String.format( "intersection is not a vertice of item '%s'", item2.getName() );

    return null;
  }

  private boolean isVerticeOf( final Polygon polygon, final Coordinate vertice )
  {
    final Coordinate[] coordinates = polygon.getExteriorRing().getCoordinates();
    for( final Coordinate crd : coordinates )
    {
      if( crd.equals2D( vertice ) )
        return true;
    }

    return false;
  }

  private String check1dim( final Geometry intersection, final IPolygonWithName item1, final IPolygonWithName item2 )
  {
    if( !(intersection instanceof LineString) )
      throw new IllegalStateException();

    final LineString edge = (LineString) intersection;
    final int numPoints = edge.getNumPoints();
    if( numPoints < 2 )
      throw new IllegalStateException();

    if( numPoints > 2 )
      return "intersect with edge of more than 2 points";

    if( edge.getLength() == 0.0 )
      return "invalid edge";

    // 2 points, edge must be a real edge of both polygons
    final LineSegment segment = new LineSegment( edge.getCoordinateN( 0 ), edge.getCoordinateN( 1 ) );
    if( !isEdgeOf( item1.getPolygon(), segment ) )
      return String.format( "intersection is not a complete edge of item '%s'", item1.getName() );

    if( !isEdgeOf( item2.getPolygon(), segment ) )
      return String.format( "intersection is not a complete edge of item '%s'", item2.getName() );

    return null;
  }

  /**
   * Check if a segment is an edge of a polygon.
   */
  private boolean isEdgeOf( final Polygon polygon, final LineSegment segment )
  {
    final LineString exteriorRing = polygon.getExteriorRing();

    final int numPoints = exteriorRing.getNumPoints();
    for( int i = 0; i < numPoints - 1; i++ )
    {
      final Coordinate crd1 = exteriorRing.getCoordinateN( i );
      final Coordinate crd2 = exteriorRing.getCoordinateN( i + 1 );
      final LineSegment otherSegment = new LineSegment( crd1, crd2 );
      if( segment.equalsTopo( otherSegment ) )
        return true;
    }

    return false;
  }
}