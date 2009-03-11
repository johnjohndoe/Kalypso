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
package org.kalypsodeegree_impl.graphics.displayelements;

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Walks along the given <tt>GM_Curve</tt> and generates positions on the line string in regular intervals (i.e. with
 * the same distance).
 * <p>
 * 
 * @author <a href="mailto:mschneider@lat-lon.de>Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public class CurveWalker
{

  private final int minX, minY, maxX, maxY;

  /**
   * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
   * @version 2.11.2002
   */
  public CurveWalker( final Rectangle bounds )
  {
    minX = (int) (bounds.getMinX() + 0.5);
    minY = (int) (bounds.getMinY() + 0.5);
    maxX = (int) (bounds.getMaxX() + 0.5);
    maxY = (int) (bounds.getMaxY() + 0.5);
    // System.out.println (minX + "," + minY + " " + maxX + "," + maxY);
  }

  /**
   * TODO: buggy. something some icon are not draw, probably due to rounding errors
   * <p>
   * Determines positions on the given <tt>GM_Curve</tt> where a caption could be drawn. For each of this positons,
   * three candidates are produced; one on the line, one above of it and one below.
   * <p>
   * 
   * @param pos
   * @return ArrayList containing Arrays of Label-objects
   */
  public List<double[]> createPositions( final int[][] pos, final double width )
  {
    // walk along the linestring and "collect" possible placement positions
    int lastX = pos[0][0];
    int lastY = pos[1][0];
    final int count = pos[2][0];

    int boxStartX = lastX;
    int boxStartY = lastY;

    final ArrayList<double[]> labels = new ArrayList<double[]>( 100 );
    final List<int[]> eCandidates = new ArrayList<int[]>( 100 );

    int i = 0;
    while( i < count )
    {
      int x = pos[0][i];
      int y = pos[1][i];

      // segment found where endpoint of box should be located?
      if( getDistance( boxStartX, boxStartY, x, y ) > width )
      {
        final int[] p0 = new int[] { boxStartX, boxStartY };
        final int[] p1 = new int[] { lastX, lastY };
        final int[] p2 = new int[] { x, y };

        final int[] p = findPointWithDistance( p0, p1, p2, width );
        x = p[0];
        y = p[1];

        lastX = x;
        lastY = y;
        final int boxEndX = x;
        final int boxEndY = y;

        final double rotation = getRotation( boxStartX, boxStartY, boxEndX, boxEndY );
        calcDeviation( new int[] { boxStartX, boxStartY }, new int[] { boxEndX, boxEndY }, eCandidates );

        // only add position if it is visible
        if( boxStartX >= minX && boxStartX <= maxX && boxStartY >= minY && boxStartY <= maxY )
          labels.add( new double[] { boxStartX, boxStartY, rotation } );

        boxStartX = lastX;
        boxStartY = lastY;
        eCandidates.clear();
      }
      else
      {
        eCandidates.add( new int[] { x, y } );
        lastX = x;
        lastY = y;
        i++;
      }
    }
    return labels;
  }

  /**
   * Calculates the maximum deviation that points on a linestring have to the ideal line between the starting point and
   * the end point.
   * <p>
   * The ideal line is thought to be running from left to right, the left deviation value generally is above the line,
   * the right value is below.
   * <p>
   * 
   * @param start
   *            starting point of the linestring
   * @param end
   *            end point of the linestring
   * @param points
   *            points in between
   */
  public double[] calcDeviation( int[] start, int[] end, final List<int[]> points )
  {

    // extreme deviation to the left
    double d1 = 0.0;
    // extreme deviation to the right
    double d2 = 0.0;
    final Iterator<int[]> it = points.iterator();

    // eventually swap start and end point
    if( start[0] > end[0] )
    {
      final int[] tmp = start;
      start = end;
      end = tmp;
    }

    if( start[0] != end[0] )
    {
      // label orientation is not completly vertical
      if( start[1] != end[1] )
      {
        // label orientation is not completly horizontal
        while( it.hasNext() )
        {
          final int[] point = it.next();
          final double u = ((double) end[1] - (double) start[1]) / ((double) end[0] - (double) start[0]);
          final double x = (u * u * start[0] - u * ((double) start[1] - (double) point[1]) + point[0]) / (1.0 + u * u);
          final double y = (x - start[0]) * u + start[1];
          final double d = getDistance( point, new int[] { (int) (x + 0.5), (int) (y + 0.5) } );
          if( y >= point[1] )
          {
            // candidate for left extreme value
            if( d > d1 )
            {
              d1 = d;
            }
          }
          else if( d > d2 )
          {
            // candidate for right extreme value
            d2 = d;
          }
        }
      }
      else
      {
        // label orientation is completly horizontal
        while( it.hasNext() )
        {
          final int[] point = it.next();
          double d = point[1] - start[1];
          if( d < 0 )
          {
            // candidate for left extreme value
            if( -d > d1 )
            {
              d1 = -d;
            }
          }
          else if( d > d2 )
          {
            // candidate for left extreme value
            d2 = d;
          }
        }
      }
    }
    else
    {
      // label orientation is completly vertical
      while( it.hasNext() )
      {
        final int[] point = it.next();
        double d = point[0] - start[0];
        if( d < 0 )
        {
          // candidate for left extreme value
          if( -d > d1 )
          {
            d1 = -d;
          }
        }
        else if( d > d2 )
        {
          // candidate for right extreme value
          d2 = d;
        }
      }
    }
    return new double[] { d1, d2 };
  }

  /**
   * Finds a point on the line between p1 and p2 that has a certain distance from point p0 (provided that there is such
   * a point).
   * <p>
   * 
   * @param p0
   *            point that is used as reference point for the distance
   * @param p1
   *            starting point of the line
   * @param p2
   *            end point of the line
   * @param d
   *            distance
   */
  public static int[] findPointWithDistance( final int[] p0, final int[] p1, final int[] p2, final double d )
  {
    double x, y;
    final double x0 = p0[0];
    final double y0 = p0[1];
    final double x1 = p1[0];
    final double y1 = p1[1];
    final double x2 = p2[0];
    final double y2 = p2[1];

    if( x1 != x2 )
    {
      // line segment does not run vertical
      final double u = (y2 - y1) / (x2 - x1);
      double p = -2 * (x0 + u * u * x1 - u * (y1 - y0)) / (u * u + 1);
      final double q = ((y1 - y0) * (y1 - y0) + u * u * x1 * x1 + x0 * x0 - 2 * u * x1 * (y1 - y0) - d * d) / (u * u + 1);
      int minX = p1[0];
      int maxX = p2[0];
      int minY = p1[1];
      int maxY = p2[1];
      if( minX > maxX )
      {
        minX = p2[0];
        maxX = p1[0];
      }
      if( minY > maxY )
      {
        minY = p2[1];
        maxY = p1[1];
      }

      if( x1 < x2 )
      {
        // to the right
        x = -p / 2 + Math.sqrt( (p / 2) * (p / 2) - q );
      }
      else
      {
        // to the left
        x = -p / 2 - Math.sqrt( (p / 2) * (p / 2) - q );
      }

      // if ((int) (x + 0.5) <= minX || (int) (x + 0.5) >= maxX) {
      // x = -p / 2 + Math.sqrt ((p / 2) * (p / 2) - q);
      // }
      y = (x - x1) * u + y1;
    }
    else
    {
      // vertical line segment
      x = x1;
      int minY = p1[1];
      int maxY = p2[1];

      if( minY > maxY )
      {
        minY = p2[1];
        maxY = p1[1];
      }

      double p = -2 * y0;
      final double q = y0 * y0 + (x1 - x0) * (x1 - x0) - d * d;

      if( y1 > y2 )
      {
        // down
        y = -p / 2 - Math.sqrt( (p / 2) * (p / 2) - q );
      }
      else
      {
        // up
        y = -p / 2 + Math.sqrt( (p / 2) * (p / 2) - q );
      }

      // y = -p / 2 - Math.sqrt ((p / 2) * (p / 2) - q);
      // if ((int) (y + 0.5) <= minY || (int) (y + 0.5) >= maxY) {
      // y = -p / 2 + Math.sqrt ((p / 2) * (p / 2) - q);
      // }
    }
    return new int[] { (int) (x + 0.5), (int) (y + 0.5) };
  }

  public double getRotation( final double x1, final double y1, final double x2, final double y2 )
  {
    final double dx = x2 - x1;
    final double dy = -(y2 - y1);
    double rotation = 0.0;

    if( dx <= 0 )
    {
      if( dy <= 0 )
      {
        // left down
        rotation = -Math.atan( dy / dx );
      }
      else
      {
        // left up
        rotation = -Math.atan( dy / dx );
      }
    }
    else
    {
      if( dy <= 0 )
      {
        // right down
        rotation = -Math.PI - Math.atan( dy / dx );
      }
      else
      {
        // right up
        rotation = -Math.PI - Math.atan( dy / dx );
      }
    }
    return rotation;
  }

  public double getDistance( final int[] p1, final int[] p2 )
  {
    final double dx = p1[0] - p2[0];
    final double dy = p1[1] - p2[1];
    return Math.sqrt( dx * dx + dy * dy );
  }

  public double getDistance( final double x1, final double y1, final double x2, final double y2 )
  {
    final double dx = x2 - x1;
    final double dy = y2 - y1;
    return Math.sqrt( dx * dx + dy * dy );
  }
}