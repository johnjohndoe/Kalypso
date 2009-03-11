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
package org.kalypso.ogc.gml.widgets.tools;

import java.awt.Color;
import java.awt.Graphics;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.runtime.Assert;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * @author Dirk Kuch
 */
public class GeometryPainter
{
  public static void highlightPoints( final Graphics g, final IMapPanel panel, final Geometry[] geometries, final IPointHighLighter hightlighter )
  {
    highlightPoints( g, panel, geometries, null, Double.NaN, hightlighter );
  }

  public static void highlightPoints( final Graphics g, final IMapPanel panel, final Geometry[] geometries, final Point base, final double radius, final IPointHighLighter hightlighter )
  {
    final Point[] points = findPointsInRange( geometries, base, radius );

    for( final Point point : points )
    {
      try
      {
        final GM_Point gmp = (GM_Point) JTSAdapter.wrap( point );
        final java.awt.Point awtPoint = MapUtilities.retransform( panel, gmp );

        hightlighter.draw( g, awtPoint );

      }
      catch( final GM_Exception e )
      {
        KalypsoCorePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
      }
    }

  }

  private static Point[] findPointsInRange( final Geometry[] geometries, final Point base, final double radius )
  {
    final Set<Point> points = new HashSet<Point>();

    for( final Geometry geometry : geometries )
    {
      if( geometry instanceof Polygon )
      {
        final Polygon polygon = (Polygon) geometry;
        final LineString ring = polygon.getExteriorRing();

        for( int i = 0; i < ring.getNumPoints(); i++ )
        {
          final Point point = ring.getPointN( i );

          if( base == null )
          {
            points.add( point );
          }
          else if( point.distance( base ) <= radius )
          {
            points.add( point );
          }
        }
      }
      else if( geometry instanceof Point )
      {
        final Point point = (Point) geometry;

        if( base == null )
        {
          points.add( point );
        }
        else if( point.distance( base ) <= radius )
        {
          points.add( point );
        }
      }
      else
        throw new NotImplementedException();

    }

    return points.toArray( new Point[] {} );
  }

  public static void drawLineString( final IMapPanel mapPanel, final Graphics g, final Point[] myPoints, final Color color ) throws GM_Exception
  {
    final List<java.awt.Point> awtPoints = new ArrayList<java.awt.Point>();

    final Set<Coordinate> coordinates = new LinkedHashSet<Coordinate>();

    for( final Point point : myPoints )
    {
      coordinates.add( point.getCoordinate() );

      final GM_Point gmp = (GM_Point) JTSAdapter.wrap( point );
      final java.awt.Point awt = MapUtilities.retransform( mapPanel, gmp );

      awtPoints.add( awt );
    }


    final Color original = g.getColor();
    g.setColor( color );

    for( int i = 0; i < awtPoints.size() - 1; i++ )
    {
      final java.awt.Point p1 = awtPoints.get( i );
      final java.awt.Point p2 = awtPoints.get( i + 1 );

      g.drawLine( Double.valueOf( p1.getX() ).intValue(), Double.valueOf( p1.getY() ).intValue(), Double.valueOf( p2.getX() ).intValue(), Double.valueOf( p2.getY() ).intValue() );
    }

    g.setColor( original );
  }

  public static void drawPolygons( final IMapPanel mapPanel, final Graphics g, final Polygon[] polygons, final Color border, final Color fill )
  {
    for( final Polygon polygon : polygons )
    {
      drawPolygon( mapPanel, g, polygon, border, fill );
    }
  }

  private static void drawPolygon( final IMapPanel mapPanel, final Graphics g, final Polygon polygon, final Color border, final Color fill )
  {
    final LineString ring = polygon.getExteriorRing();
    int[] x = new int[] {};
    int[] y = new int[] {};
    for( int i = 0; i < ring.getNumPoints(); i++ )
    {

      try
      {
        final Point p = ring.getPointN( i );
        final GM_Point gmp = (GM_Point) JTSAdapter.wrap( p );

        final java.awt.Point awt = MapUtilities.retransform( mapPanel, gmp );
        x = ArrayUtils.add( x, awt.x );
        y = ArrayUtils.add( y, awt.y );
      }
      catch( final GM_Exception e )
      {
        KalypsoCorePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
      }
    }

    Assert.isTrue( x.length == y.length );

    final java.awt.Polygon poly = new java.awt.Polygon( x, y, x.length );

    final Color original = g.getColor();

    g.setColor( fill );
    g.fillPolygon( poly );

    g.setColor( border );
    g.drawPolygon( poly );

    g.setColor( original );
    
  }
}
