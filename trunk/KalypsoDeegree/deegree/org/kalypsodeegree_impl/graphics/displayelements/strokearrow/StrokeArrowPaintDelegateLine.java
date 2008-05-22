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
package org.kalypsodeegree_impl.graphics.displayelements.strokearrow;

import java.awt.Graphics2D;

import org.apache.commons.lang.NotImplementedException;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.graphics.displayelements.strokearrow.StrokeArrowHelper.ARROW_ALIGNMENT;
import org.kalypsodeegree_impl.graphics.displayelements.strokearrow.StrokeArrowHelper.ARROW_TYPE;
import org.kalypsodeegree_impl.graphics.displayelements.strokearrow.StrokeArrowHelper.ARROW_WIDGET;
import org.kalypsodeegree_impl.graphics.displayelements.strokearrow.geometries.AbstractArrowGeometry;
import org.kalypsodeegree_impl.graphics.displayelements.strokearrow.geometries.IArrowGeometry;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author kuch
 */
public class StrokeArrowPaintDelegateLine extends AbstractStrokeArrowPaintDelegate
{

  private static final double MIN_DISTANCE_OF_POINTS = 4.0; /*
                                                             * REMARK: GM_POINTS will be transfered to screen
                                                             * coordinates, and these points must differ!
                                                             */

  public StrokeArrowPaintDelegateLine( final ARROW_TYPE arrowType, final ARROW_WIDGET arrowWidget, final ARROW_ALIGNMENT arrowAlignment, final Double arrowSize, final Double strokeWidth )
  {
    super( arrowType, arrowWidget, arrowAlignment, arrowSize );
  }

  /**
   * @see org.kalypsodeegree_impl.graphics.displayelements.strokearrow.IStrokeArrowPaintDelegate#paint(org.kalypsodeegree_impl.graphics.sld.awt.StrokePainter,
   *      java.awt.Graphics2D, org.kalypsodeegree.graphics.transformation.GeoTransform,
   *      org.kalypsodeegree.model.geometry.GM_Curve)
   */
  public void paint( final Graphics2D g2, final GeoTransform projection, final GM_Curve curve, final UOM uom )
  {
    try
    {
      final GM_Point[] points = calculatePoints( curve );

      final IArrowGeometry arrow = AbstractArrowGeometry.getArrowGeometry( getWidget(), g2, projection, points, uom );
      arrow.paint( getSize() );

      // draw triangle (arrow)
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }

  }

  private GM_Point[] calculatePoints( final GM_Curve curve ) throws GM_Exception
  {
    switch( getAlignment() )
    {
      case eStart:
        return calculateStartPoints( curve );

      case eEnd:
        return calculateEndPoints( curve );

      case eMiddle:
        return calculateCenterPoints( curve );
    }

    return null;
  }

  private GM_Point[] calculateStartPoints( final GM_Curve curve ) throws GM_Exception
  {
    final GM_LineString lineString = curve.getAsLineString();
    final GM_Position[] positions = lineString.getPositions();

    if( positions.length < 2 )
      throw new IllegalStateException();

    final GM_Point a = GeometryFactory.createGM_Point( positions[0], curve.getCoordinateSystem() );
    GM_Point b = a;

    /* point b must differ from point a */
    int count = 1;
    double distance = 0.0;

    while( b.equals( a ) || distance < MIN_DISTANCE_OF_POINTS )
    {
      b = GeometryFactory.createGM_Point( positions[count], curve.getCoordinateSystem() );
      count++;

      distance = b.distance( a );
    }

    return new GM_Point[] { a, b };

  }

  private GM_Point[] calculateEndPoints( final GM_Curve curve ) throws GM_Exception
  {
    final GM_LineString lineString = curve.getAsLineString();
    final GM_Position[] positions = lineString.getPositions();

    if( positions.length < 2 )
      throw new IllegalStateException();

    final GM_Point a = GeometryFactory.createGM_Point( positions[positions.length - 1], curve.getCoordinateSystem() );
    GM_Point b = a;

    /* point b must differ from point a! */
    int count = 2;
    double distance = 0.0;

    while( b.equals( a ) || distance < MIN_DISTANCE_OF_POINTS )
    {
      if( count > positions.length )

      {
        if( a.equals( b ) )
          return new GM_Point[] {};
        return new GM_Point[] { a, b };
      }
      else
      {
        b = GeometryFactory.createGM_Point( positions[positions.length - count], curve.getCoordinateSystem() );
        count++;

        distance = b.distance( a );
      }
    }

    return new GM_Point[] { a, b };
  }

  private GM_Point[] calculateCenterPoints( final GM_Curve curve )
  {
    // TODO JTSUtilis.pointOnPercent - getcenterpoint

    throw new NotImplementedException();
  }

}
