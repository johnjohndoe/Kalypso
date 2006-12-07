/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ogc.gml.map.widgets.builders;

import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * This class is a geometry builder for a line.
 * 
 * @author Holger Albert
 */
public class LineGeometryBuilder implements IGeometryBuilder
{
  /**
   * Stores the count of points which this geometry must have. If it is 0, there is no rule.
   */
  private int m_cnt_points;

  private List<GM_Point> m_points = new ArrayList<GM_Point>();

  private final CS_CoordinateSystem m_crs;

  /**
   * The constructor.
   * 
   * @param cnt_points
   *          If >0 the the geometry will be finished, if the count of points is reached. If 0 no rule regarding the
   *          count of the points will apply.
   * @param targetCrs
   *          The target coordinate system.
   */
  public LineGeometryBuilder( final int cnt_points, final CS_CoordinateSystem targetCrs )
  {
    m_cnt_points = 0;

    if( cnt_points >= 0 )
      m_cnt_points = cnt_points;

    m_crs = targetCrs;
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.IGeometryBuilder#addPoint(org.kalypsodeegree.model.geometry.GM_Position)
   */
  public GM_Object addPoint( final GM_Point p ) throws Exception
  {
    m_points.add( p );

    if( m_points.size() == m_cnt_points )
      return finish();

    return null;
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.IGeometryBuilder#finish()
   */
  public GM_Object finish( ) throws Exception
  {
    if( (m_points.size() == m_cnt_points) || (m_cnt_points == 0) )
    {
      final GeoTransformer transformer = new GeoTransformer( m_crs );

      final GM_Position[] poses = new GM_Position[m_points.size()];
      for( int i = 0; i < poses.length; i++ )
      {
        final GM_Point transformedPoint = (GM_Point) transformer.transform( m_points.get( i ) );
        poses[i] = transformedPoint.getPosition();
      }

      return createGeometry( poses );
    }

    return null;
  }

  /**
   * This function creates the geometry (GM_Curve).
   */
  private GM_Object createGeometry( final GM_Position[] poses ) throws GM_Exception
  {
    return GeometryFactory.createGM_Curve( poses, m_crs );
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.IGeometryBuilder#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform)
   */
  public void paint( final Graphics g, final GeoTransform projection, final Point currentPoint )
  {
    // IMPORTANT: we remeber GM_Points (not Point's) and retransform them for painting
    // because the projection depends on the current map-extent, so this builder
    // is stable in regard to zoom in/out
    if( !m_points.isEmpty() )
    {
      final int[][] points = getPointArrays( projection, currentPoint );

      final int[] arrayX = points[0];
      final int[] arrayY = points[1];

      /* Paint a linestring. */
      g.drawPolyline( arrayX, arrayY, arrayX.length );
      drawHandles( g, arrayX, arrayY );
    }
  }

  private int[][] getPointArrays( final GeoTransform projection, final Point currentPoint )
  {
    final List<Integer> xArray = new ArrayList<Integer>();
    final List<Integer> yArray = new ArrayList<Integer>();

    for( int i = 0; i < m_points.size(); i++ )
    {
      final GM_Point point = m_points.get( i );

      final int x = (int) projection.getDestX( point.getX() );
      final int y = (int) projection.getDestY( point.getY() );

      xArray.add( new Integer( x ) );
      yArray.add( new Integer( y ) );
    }

    if( currentPoint != null )
    {
      xArray.add( currentPoint.x );
      yArray.add( currentPoint.y );
    }

    final int[] xs = ArrayUtils.toPrimitive( xArray.toArray( new Integer[xArray.size()] ) );
    final int[] ys = ArrayUtils.toPrimitive( yArray.toArray( new Integer[yArray.size()] ) );

    return new int[][] { xs, ys };
  }

  private void drawHandles( final Graphics g, final int[] x, final int[] y )
  {
    int sizeOuter = 6;
    for( int i = 0; i < y.length; i++ )
      g.drawRect( x[i] - sizeOuter / 2, y[i] - sizeOuter / 2, sizeOuter, sizeOuter );
  }

}
