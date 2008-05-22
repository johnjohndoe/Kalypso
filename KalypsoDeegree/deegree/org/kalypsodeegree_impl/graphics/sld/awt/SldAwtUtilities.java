/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.graphics.sld.awt;

import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.Shape;
import java.awt.geom.Area;

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * Helper class for ocnversion beetween GM_Object and awt-Shapes.
 * 
 * @author Gernot Belger
 */
public final class SldAwtUtilities
{
  private SldAwtUtilities( )
  {
    throw new UnsupportedOperationException( "Helper class, do not instantiate" );
  }

  /**
   * Creates an awt {@link Polygon} from the given ring, transforming it to screen coordinates.
   * <p>
   * For perfomance reasons, points which cannot be distuingished on the screen are filtered out.
   * </p>
   * 
   * @param strokeWidth:
   *            Width in screen units: any adjacent points which are within this distance are filtered out.
   */
  public static Polygon polygonFromRing( final GM_Position[] ring, final double strokeWidth, final GeoTransform world2screen )
  {
    final int[] x = new int[ring.length];
    final int[] y = new int[ring.length];

    int k = 0;
    for( final GM_Position element : ring )
    {
      final GM_Position position = world2screen.getDestPoint( element );
      final int xx = (int) (position.getX() + 0.5);
      final int yy = (int) (position.getY() + 0.5);

      if( k > 0 && k < ring.length - 1 )
      {
        // PERFORMANCE: We ignore points whichs distance is smaller than 'm_width'
        // TODO: maybe even round to next smaller int? May enhance if width == 0
        if( distance( xx, yy, x[k - 1], y[k - 1] ) > strokeWidth )
        {
          x[k] = xx;
          y[k] = yy;
          k++;
        }
      }
      else
      {
        x[k] = xx;
        y[k] = yy;
        k++;
      }
    }

    return new Polygon( x, y, k - 1 );
  }

  /**
   * Same as {@link #polygonFromRing(GM_Position[], double, GeoTransform)}, but makes an {@link Area} from the
   * {@link Polygon} before returning it.
   */
  public static Area areaFromRing( final GM_Position[] ring, final double strokeWidth, final GeoTransform world2screen )
  {
    return new Area( polygonFromRing( ring, strokeWidth, world2screen ) );
  }

  /**
   * Creates an {@link Area} with holes from an exterior ring and interior rings.
   */
  public static <T extends GM_SurfacePatch> Shape shapeFromSurface( final GM_Surface<T> surface, final double strokeWidth, final GeoTransform world2screen )
  {
    final GM_SurfacePatch patch = surface.get( 0 );
    final GM_Position[] outerRing = patch.getExteriorRing();
    final GM_Position[][] innerRings = patch.getInteriorRings();

    try
    {
      // OPTIMIZATION: If we have no inner rings, only create a polygone, this is much faster.
      if( innerRings == null || innerRings.length == 0 )
        return polygonFromRing( outerRing, strokeWidth, world2screen );

      // TODO: slow! Maybe at least test if inner rings are visible?
      final Area areaouter = areaFromRing( outerRing, strokeWidth, world2screen );
      if( innerRings != null )
      {
        for( final GM_Position[] innerRing : innerRings )
        {
          if( innerRing != null )
            areaouter.subtract( areaFromRing( innerRing, strokeWidth, world2screen ) );
        }
      }

      return areaouter;
    }
    catch( final Exception e )
    {
      Debug.debugException( e, "" );
    }

    return null;
  }

  private static double distance( final double x1, final double y1, final double x2, final double y2 )
  {
    return Math.sqrt( (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) );
  }

  public static void paintRing( final Graphics2D gc, final GM_Position[] ring, final GeoTransform world2screen, final FillPainter fillPainter, final StrokePainter strokePainter ) throws Exception
  {
    final Shape area = SldAwtUtilities.polygonFromRing( ring, strokePainter.getWidth(), world2screen );
    paintShape( gc, area, fillPainter, strokePainter );
  }

  public static void paintShape( final Graphics2D g2, final Shape shape, final FillPainter fillPainter, final StrokePainter strokePainter ) throws Exception
  {
    if( fillPainter != null && fillPainter.isVisible() )
    {
      fillPainter.prepareGraphics( g2 );
      g2.fill( shape );
    }

    if( strokePainter != null && strokePainter.isVisible() )
    {
      strokePainter.prepareGraphics( g2 );
      g2.draw( shape );
    }
  }

  public static void paintSurface( final Graphics2D g2, final GM_Surface< ? > surface, final GeoTransform world2screen, final FillPainter fillPainter, final StrokePainter strokePainter ) throws Exception
  {
    final Shape shape = SldAwtUtilities.shapeFromSurface( surface, strokePainter.getWidth(), world2screen );

    paintShape( g2, shape, fillPainter, strokePainter );
  }
}
