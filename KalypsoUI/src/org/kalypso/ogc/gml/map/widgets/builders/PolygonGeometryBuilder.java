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
package org.kalypso.ogc.gml.map.widgets.builders;

import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.transformation.GeoTransformer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * This class is a geometry builder for a polygon.
 * 
 * @author Holger Albert
 */
public class PolygonGeometryBuilder implements IGeometryBuilder
{
  /**
   * Stores the count of points which this geometry must have. If it is 0, there is no rule.
   */
  private int m_cnt_points;

  private final List<GM_Point> m_points = new ArrayList<GM_Point>();

  private final String m_crs;

  private GM_Object m_result;

  private final ToolTipRenderer m_renderer;

  final java.awt.Cursor CROSSHAIR_CURSOR = java.awt.Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR );

  final java.awt.Cursor DEFAULT_CURSOR = java.awt.Cursor.getPredefinedCursor( Cursor.DEFAULT_CURSOR );

  private final IGeometryBuilderExtensionProvider m_extender;

  /**
   * The constructor.
   * 
   * @param cnt_points
   *            If > 2 the the geometry will be finished, if the count of points is reached. If <= 2 no rule regarding
   *            the count of the points will apply, except, that a polygon needs at least 3 points for beeing created.
   * @param targetCrs
   *            The target coordinate system.
   */
  public PolygonGeometryBuilder( final int cnt_points, final String targetCrs, final IGeometryBuilderExtensionProvider extender )
  {
    m_extender = extender;
    m_cnt_points = 0;

    if( cnt_points > 2 )
      m_cnt_points = cnt_points;

    m_crs = targetCrs;

    m_renderer = new ToolTipRenderer( m_extender );

    if( m_extender != null )
      m_extender.setCursor( CROSSHAIR_CURSOR );
  }

  /**
   * The constructor.
   * 
   * @param cnt_points
   *            If > 2 the the geometry will be finished, if the count of points is reached. If <= 2 no rule regarding
   *            the count of the points will apply, except, that a polygon needs at least 3 points for beeing created.
   * @param targetCrs
   *            The target coordinate system.
   */
  public PolygonGeometryBuilder( final int cnt_points, final String targetCrs )
  {
    this( cnt_points, targetCrs, null );
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.IGeometryBuilder#addPoint(org.kalypsodeegree.model.geometry.GM_Position)
   */
  public GM_Object addPoint( final GM_Point p ) throws Exception
  {
    m_points.add( p );

    if( m_points.size() > 2 && m_points.size() == m_cnt_points )
      return finish();

    return null;
  }

  /**
   * This function creates the geometry (GM_Surface).
   */
  private GM_Object createGeometry( final GM_Position[] poses ) throws GM_Exception
  {
    final GM_Position[] pos = new GM_Position[poses.length + 1];

    for( int i = 0; i < poses.length; i++ )
      pos[i] = poses[i];

    /*
     * REMARK: Need a cloned point, otherwise changes to one point affects the other two, if the workspace containing
     * the feature is not completely reloaded.
     */
    final double[] asArray = poses[0].getAsArray();
    final double[] newArray = new double[asArray.length];

    for( int i = 0; i < asArray.length; i++ )
      newArray[i] = asArray[i];

    final GM_Position newPoint = GeometryFactory.createGM_Position( newArray );
    pos[poses.length] = newPoint;

    return GeometryFactory.createGM_Surface( pos, new GM_Position[0][0], null, m_crs );
  }

  private void drawHandles( final Graphics g, final int[] x, final int[] y )
  {
    final int sizeOuter = 6;
    for( int i = 0; i < y.length; i++ )
      g.drawRect( x[i] - sizeOuter / 2, y[i] - sizeOuter / 2, sizeOuter, sizeOuter );
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.IGeometryBuilder#finish()
   */
  public GM_Object finish( ) throws Exception
  {
    if( m_extender != null )
      m_extender.setCursor( DEFAULT_CURSOR );

    if( m_result != null )
      return m_result;

    if( (m_points.size() > 2) && ((m_cnt_points == m_points.size()) || (m_cnt_points <= 2)) )
    {
      final GeoTransformer transformer = new GeoTransformer( m_crs );

      final GM_Position[] poses = new GM_Position[m_points.size()];
      for( int i = 0; i < poses.length; i++ )
      {
        final GM_Point transformedPoint = (GM_Point) transformer.transform( m_points.get( i ) );
        poses[i] = transformedPoint.getPosition();
      }

// m_result = createGeometry( poses );
// return m_result;
      return createGeometry( poses );
    }

    return null;
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

  /**
   * @see org.kalypso.informdss.manager.util.widgets.IGeometryBuilder#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform)
   */
  public void paint( final Graphics g, final GeoTransform projection, final Point currentPoint )
  {
    // IMPORTANT: we remember GM_Points (not Point's) and retransform them for painting
    // because the projection depends on the current map-extent, so this builder
    // is stable in regard to zoom in/out
    if( !m_points.isEmpty() )
    {
      final int[][] points = getPointArrays( projection, currentPoint );

      final int[] arrayX = points[0];
      final int[] arrayY = points[1];

      /* Paint a polygon. */
      g.drawPolygon( arrayX, arrayY, arrayX.length );
      drawHandles( g, arrayX, arrayY );
    }

    m_renderer.paint( g );
  }

  /**
   * @see org.kalypso.nofdpidss.ui.view.wizard.measure.construction.create.builders.geometry.IMyGeometryBuilder#removeLastPoint()
   */
  public void removeLastPoint( )
  {
    if( m_points.size() > 0 )
      m_points.remove( m_points.size() - 1 );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.builders.IGeometryBuilder#removePoints()
   */
  public void reset( )
  {
    m_points.clear();
    m_result = null;
    if( m_extender != null )
      m_extender.setCursor( CROSSHAIR_CURSOR );
  }
}