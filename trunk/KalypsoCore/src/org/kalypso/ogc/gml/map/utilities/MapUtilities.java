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
package org.kalypso.ogc.gml.map.utilities;

import java.awt.Point;

import org.kalypso.jts.SnapUtilities;
import org.kalypso.jts.SnapUtilities.SNAP_TYPE;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.cs.CS_CoordinateSystem;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Utility class for map operations.
 * 
 * @author Holger Albert
 */
public class MapUtilities
{
  private MapUtilities( )
  {
  }

  /**
   * Snaps the given AWT-Point to a given geometry, if it lies into a specified radius.
   * 
   * @param mapPanel
   *          The MapPanel of the map.
   * @param p
   *          The AWT-Point which should be snapped.
   * @param radiusPx
   *          This radius will be converted to a world coord radius. Within this circle, the AWT-Point is beeing
   *          snapped.
   * @param type
   *          This type of snapping will be used. {@link SNAP_TYPE}
   * @return The GM_Point snapped on the geometry.
   */
  public static GM_Point snap( MapPanel mapPanel, GM_Object geometry, Point p, int radiusPx, SNAP_TYPE type ) throws GM_Exception
  {
    /* Get the JTS geometry. */
    Geometry geometryJTS = JTSAdapter.export( geometry );

    /* Transform the point to a GM_Point. */
    GM_Point point = MapUtilities.transform( mapPanel, p );
    com.vividsolutions.jts.geom.Point pointJTS = (com.vividsolutions.jts.geom.Point) JTSAdapter.export( point );

    /* Buffer the point. */
    Geometry pointBuffer = pointJTS.buffer( MapUtilities.calculateWorldDistance( mapPanel, point, radiusPx ) );

    if( !pointBuffer.intersects( geometryJTS ) )
      return null;

    if( geometryJTS instanceof com.vividsolutions.jts.geom.Point )
    {
      com.vividsolutions.jts.geom.Point snapPoint = SnapUtilities.snapPoint( pointJTS );
      if( snapPoint != null )
        return (GM_Point) JTSAdapter.wrap( snapPoint );
    }
    else if( geometryJTS instanceof LineString )
    {
      com.vividsolutions.jts.geom.Point snapPoint = SnapUtilities.snapLine( (LineString) geometryJTS, pointBuffer, type );
      if( snapPoint != null )
        return (GM_Point) JTSAdapter.wrap( snapPoint );
    }
    else if( geometryJTS instanceof Polygon )
    {
      com.vividsolutions.jts.geom.Point snapPoint = SnapUtilities.snapPolygon( (Polygon) geometryJTS, pointBuffer, type );
      if( snapPoint != null )
        return (GM_Point) JTSAdapter.wrap( snapPoint );
    }

    return null;
  }

  /**
   * This method transforms the AWT-Point to a GM_Point.
   * 
   * @param p
   *          The AWT-Point.
   */
  public static GM_Point transform( MapPanel mapPanel, Point p )
  {
    final GeoTransform projection = mapPanel.getProjection();
    final CS_CoordinateSystem coordinatesSystem = mapPanel.getMapModell().getCoordinatesSystem();

    final double x = p.getX();
    final double y = p.getY();

    return GeometryFactory.createGM_Point( projection.getSourceX( x ), projection.getSourceY( y ), coordinatesSystem );
  }

  /**
   * This method transforms the GM_Point to a AWT-Point.
   * 
   * @param p
   *          The GM_Point.
   */
  public static Point retransform( MapPanel mapPanel, GM_Point p )
  {
    final GeoTransform projection = mapPanel.getProjection();

    final double x = p.getX();
    final double y = p.getY();

    return new Point( (int) projection.getDestX( x ), (int) projection.getDestY( y ) );
  }

  /**
   * This function transforms a distance in pixel to the world distance.
   * 
   * @param mapPanel
   *          The MapPanel of the map.
   * @param reference
   *          The reference point.
   * @param distancePx
   *          The distance to be calculated.
   * @return The distance in the world coords.
   */
  public static double calculateWorldDistance( MapPanel mapPanel, GM_Point reference, int distancePx )
  {
    Point point = retransform( mapPanel, reference );
    point.x = point.x + distancePx;

    GM_Point destination = transform( mapPanel, point );
    return destination.getX() - reference.getX();
  }
}