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
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author kuch
 */
public class StrokeArrowPaintDelegateLine extends AbstractStrokeArrowPaintDelegate
{

  public StrokeArrowPaintDelegateLine( final ARROW_TYPE arrowType, final ARROW_WIDGET arrowWidget, final ARROW_ALIGNMENT arrowAlignment, final Double arrowSize, final Double strokeWidth )
  {
    super( arrowType, arrowWidget, arrowAlignment, arrowSize );
  }

  /**
   * @see org.kalypsodeegree_impl.graphics.displayelements.strokearrow.IStrokeArrowPaintDelegate#paint(org.kalypsodeegree_impl.graphics.sld.awt.StrokePainter,
   *      java.awt.Graphics2D, org.kalypsodeegree.graphics.transformation.GeoTransform,
   *      org.kalypsodeegree.model.geometry.GM_Curve)
   */
  public void paint( final Graphics2D g2, final GeoTransform projection, final GM_Curve curve )
  {
    try
    {
      final GM_Point[] points = calculatePoints( curve );

      final IArrowGeometry arrow = AbstractArrowGeometry.getArrowGeometry( getWidget(), g2, projection, points );
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
    while( b.equals( a ) )
    {
      b = GeometryFactory.createGM_Point( positions[count], curve.getCoordinateSystem() );
      count++;
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
    while( b.equals( a ) )
    {
      b = GeometryFactory.createGM_Point( positions[positions.length - count], curve.getCoordinateSystem() );
      count++;
    }

    return new GM_Point[] { a, b };
  }

  private GM_Point[] calculateCenterPoints( final GM_Curve curve )
  {
    // TODO JTSUtilis.pointOnPercent - getcenterpoint

    throw new NotImplementedException();
  }

}
