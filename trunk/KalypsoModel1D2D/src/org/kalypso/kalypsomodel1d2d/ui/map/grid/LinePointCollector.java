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
package org.kalypso.kalypsomodel1d2d.ui.map.grid;

import java.awt.Color;
import java.awt.Graphics;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.sld.CssParameter;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Stroke_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * This class is a geometry builder for a line.
 * 
 * @author Patrice Congo
 */
public class LinePointCollector
{
  /**
   * Stores the count of points which this geometry must have. If it is 0, there is no rule.
   */
  private int m_maxPoints;

  private final List<MutableGMPoint> m_points = new ArrayList<>();

  private String m_crs;

  private boolean m_isSelected = false;

  private final int highlightDeltaX = 1;

  private int m_selection = -1;

  /**
   * The constructor.
   * 
   * @param cnt_points
   *          If >0 the the geometry will be finished, if the count of points is reached. If 0 no rule regarding the
   *          count of the points will apply.
   * @param targetCrs
   *          The target coordinate system.
   */
  public LinePointCollector( final int maxCount, final String targetCrs )
  {
    m_maxPoints = 0;

    if( maxCount >= 0 )
      m_maxPoints = maxCount;

    m_crs = targetCrs;
  }

  public GM_Object addPoint( GM_Point p )
  {
    if( !(p instanceof MutableGMPoint) )
      p = new MutableGMPoint( p );

    m_points.add( (MutableGMPoint)p );

    if( m_points.size() == m_maxPoints )
      return finish();
    else
      return null;
  }

  public GM_Object finish( )
  {
    final int size = m_points.size();

    if( size == 0 )
    {
      // to avoid empty lines
      return null;
    }

    if( ((m_points.size() == m_maxPoints) && m_maxPoints != 0) || (m_maxPoints == 0) )
      return getLastPoint();

    System.out.println( "Max count not reached" ); //$NON-NLS-1$
    return null;
  }

  public void paint( final Graphics g, final GeoTransform projection, final GM_Point currentPoint, final int pointRectSize )
  {
    // IMPORTANT: we remeber GM_Points (not Point's) and retransform them for painting
    // because the projection depends on the current map-extent, so this builder
    // is stable in regard to zoom in/out
    if( !m_points.isEmpty() )
    {
      if( m_isSelected )
      {
        final int[][] points = getPointArrays( projection, null );
        final int[][] polygonPoints = toPolygonPoints( points, highlightDeltaX );

        g.drawPolygon( polygonPoints[0], polygonPoints[1], polygonPoints[0].length );
        g.fillPolygon( polygonPoints[0], polygonPoints[1], polygonPoints[0].length );

        drawHandles( g, points[0], points[1], pointRectSize, m_selection );
      }
      else
      {
        // draw a line
        final int[][] points = getPointArrays( projection, currentPoint );
        final int[] arrayX = points[0];
        final int[] arrayY = points[1];

        /* Paint a linestring. */
        g.drawPolyline( arrayX, arrayY, arrayX.length );

        drawHandles( g, arrayX, arrayY, pointRectSize, m_selection );
      }
    }
  }

  private static final int[][] toPolygonPoints( final int[][] originalPoint, final int deltaY )
  {
    if( originalPoint == null )
    {
      Assert.throwIAEOnNull( originalPoint, null );
    }

    final int SIZE = originalPoint[0].length;
    if( SIZE == 0 )
    {
      return new int[][] {};
    }
    else
    {
      final int[][] polyPoints = new int[2][SIZE + SIZE];

      for( int i = 0, opposite = SIZE + SIZE - 1; i < SIZE; i++, opposite-- )
      {
        polyPoints[0][i] = originalPoint[0][i];
        polyPoints[1][i] = originalPoint[1][i] + deltaY;

        polyPoints[0][opposite] = originalPoint[0][i];
        polyPoints[1][opposite] = originalPoint[1][i] - deltaY;
      }
      return polyPoints;
    }
  }

  private int[][] getPointArrays( final GeoTransform projection, final GM_Point currentPoint )
  {
    final List<Integer> xArray = new ArrayList<>();
    final List<Integer> yArray = new ArrayList<>();

    for( int i = 0; i < m_points.size(); i++ )
    {
      final GM_Point point = m_points.get( i );

      final int x = (int)projection.getDestX( point.getX() );
      final int y = (int)projection.getDestY( point.getY() );

      xArray.add( new Integer( x ) );
      yArray.add( new Integer( y ) );
    }

    if( currentPoint != null )
    {
      final int x = (int)projection.getDestX( currentPoint.getX() );
      final int y = (int)projection.getDestY( currentPoint.getY() );

      xArray.add( new Integer( x ) );
      yArray.add( new Integer( y ) );
    }

    final int[] xs = ArrayUtils.toPrimitive( xArray.toArray( new Integer[xArray.size()] ) );
    final int[] ys = ArrayUtils.toPrimitive( yArray.toArray( new Integer[yArray.size()] ) );

    return new int[][] { xs, ys };
  }

  private static final void drawHandles( final Graphics g, final int[] x, final int[] y, final int pointRectWidth, final int selection )
  {
    final Color oldColor = g.getColor();
    g.setColor( oldColor.darker() );
    // int sizeOuter = 4;
    final int halfRectWidth = pointRectWidth / 2;

    if( selection < 0 )
    {
      for( int i = 0; i < y.length; i++ )
      {
        g.drawRect( x[i] - halfRectWidth, y[i] - halfRectWidth, pointRectWidth, pointRectWidth );
      }
    }
    else
    {
      int i;
      for( i = 0; i < selection; i++ )
      {
        g.drawRect( x[i] - halfRectWidth, y[i] - halfRectWidth, pointRectWidth, pointRectWidth );
      }
      // is is now selection
      g.drawOval( x[i] - pointRectWidth, y[i] - pointRectWidth, pointRectWidth + pointRectWidth, pointRectWidth + pointRectWidth );
      i++;
      for( ; i < y.length; i++ )
      {
        g.drawRect( x[i] - halfRectWidth, y[i] - halfRectWidth, pointRectWidth, pointRectWidth );
      }
    }
    g.setColor( oldColor );
  }

  /**
   * removes all points in this {@link LineGeometryBuilder}
   */
  void clear( )
  {
    m_points.clear();
    // m_maxPoints = 0;
  }

  void reset( final String crs )
  {
    m_points.clear();
    m_isSelected = false;
    // m_maxPoints = 0;
    m_crs = crs;
  }

  void removeLastPoint( final boolean doDelFirstPoint )
  {
    final int index = m_points.size() - 1;

    if( index > 0 )
    {
      m_points.remove( index );
    }
    else if( index == 0 )
    {
      if( doDelFirstPoint )
      {
        m_points.remove( index );
      }
    }
  }

  /**
   * Conevniance methode to get a new builder with the same required number of point and coordinate reference system
   * 
   * @param return a new builder
   */
  public LinePointCollector getNewBuilder( )
  {
    return new LinePointCollector( m_maxPoints, m_crs );
  }

  /**
   * Returns the last point in this {@link LineGeometryBuilder}
   * 
   * @return the last point included in this {@link LineGeometryBuilder}
   */
  public GM_Point getLastPoint( )
  {
    final int size = m_points.size();
    if( size > 0 )
    {
      return m_points.get( size - 1 );
    }
    else
    {
      return null;
    }
  }

  /**
   * To get the first point included in this geometry builder
   * 
   * @return the first point in this geometry builder
   */
  public GM_Point getFirstPoint( )
  {
    final int size = m_points.size();

    if( size > 0 )
    {
      return m_points.get( 0 );
    }
    else
    {
      return null;
    }
  }

  /**
   * To set the number of points the this line geometry is required to have to be considered finished
   * 
   * @param cntPoints
   *          the new required number of points
   */
  public void setCntPoints( final int cntPoints )
  {
    m_maxPoints = cntPoints;
    final int SIZE = m_points.size();

    if( SIZE > cntPoints && cntPoints != 0 )
    {
      // trim the size to cnt_points if the array
      // already contain more than cntPoints elements

      for( int i = SIZE - 1; i >= cntPoints; i-- )
      {
        System.out.println( "Removing extra points:" + i ); //$NON-NLS-1$
        m_points.remove( i );
      }
    }
  }

  public int getPointCnt( )
  {
    return m_maxPoints;
//    return m_points.size();
  }

  /**
   * Return the number of points already in this {@link LineGeometryBuilder}
   * 
   * @return the number of points allready included in the line geometry
   */
  public int getCurrentPointCnt( )
  {
    return m_points.size();
  }

  /**
   * Return the remaining number of point to add to this {@link LineGeometryBuilder} to reach the expected number of
   * points
   * 
   * @return the actual of number of point remaining for completion or {@link Integer#MAX_VALUE} if the required number
   *         of point was set to a zero or negativ integer
   */
  public int getRemainingPointCnt( )
  {
    if( m_maxPoints <= 0 )
    {
      return Integer.MAX_VALUE;
    }
    else
    {
      return m_maxPoints - m_points.size();
    }
  }

  public void replaceLastPoint( final GM_Point point )
  {
    final int index = m_points.size() - 1;
    if( index > 0 )
    {
      // m_points.set( index, point );
      m_points.get( index ).setPoint( point );
    }
    else
    {
      if( point instanceof MutableGMPoint )
      {
        m_points.add( (MutableGMPoint)point );
      }
      else
      {
        m_points.add( new MutableGMPoint( point ) );
      }
    }
  }

  public void changeSelected( final GM_Point point )
  {
    if( m_selection >= 0 )
    {
      // m_points.set( selection, point );
      m_points.get( m_selection ).setPoint( point );

    }
    else
    {
      System.out.println( "selection=" + m_selection ); //$NON-NLS-1$
    }

  }

  public GM_Point getSelectedPoint( )
  {
    if( m_selection >= 0 )
    {
      return m_points.get( m_selection );
    }
    else
    {
      return null;
    }

  }

  public GM_Point getPointAt( final int index )
  {
    return m_points.get( index );
  }

  public void clearSelection( )
  {
    m_selection = -1;
  }

  public int selectPoint( final GM_Point lowerCorner, final double squareWidth )
  {
    if( !m_isSelected )
    {
      m_selection = -1;
      return -1;
    }

    final double halfWidth = squareWidth / 2;
    final double lowX = lowerCorner.getX() - halfWidth;
    final double lowY = lowerCorner.getY() - halfWidth;

    final double upperX = lowX + squareWidth;
    final double upperY = lowY + squareWidth;

    final double distance = Double.MAX_VALUE;
    int nextSel = -1;

    for( int i = 0; i < m_points.size(); i++ )
    {
      final GM_Point curPoint = m_points.get( i );
      final double curCoordX = curPoint.getX();
      if( lowX <= curCoordX && upperX >= curCoordX )
      {
        final double curCoordY = curPoint.getY();

        if( lowY <= curCoordY && upperY >= curCoordY )
        {
          if( distance > curPoint.distance( lowerCorner ) )
          {
            nextSel = i;
          }

        }
      }
    }

    m_selection = nextSel;
    return m_selection;
  }

  /**
   * Mark the {@link LineGeometryBuilder} as selected
   * 
   * @param isSelected
   *          -- a boolean expressing the selection state of the {@link LineGeometryBuilder}
   */
  public void setSelected( final boolean isSelected )
  {
    m_isSelected = isSelected;
    if( !isSelected )
    {
      m_selection = -1;
    }
  }

  // TODO move this method to the grid colector
  public double getHandleWidthAsWorldDistance( final IMapPanel mapPanel, final int pointRectSize )
  {
    if( m_points.size() > 0 )
      return MapUtilities.calculateWorldDistance( mapPanel, m_points.get( 0 ), pointRectSize );
    else
      return 0;
  }

  @Deprecated
  public void removeMaxNum( )
  {
//    m_maxPoints = 0;
  }

  public boolean isSelected( )
  {
    return m_isSelected;
  }

  public void paintLine( final Graphics g, final GeoTransform projection, final double width, final Color color )
  {
    final GM_Position[] positions = new GM_Position[m_points.size()];

    for( int i = 0; i < positions.length; i++ )
      positions[i] = m_points.get( i ).getPosition();

    try
    {
      final GM_Curve curve = GeometryFactory.createGM_Curve( positions, m_crs );
      if( curve == null )
        return;

      final LineSymbolizer symb = new LineSymbolizer_Impl();
      final Stroke stroke = new Stroke_Impl( new HashMap<String, CssParameter>(), null, null );

      stroke.setWidth( width );
      stroke.setStroke( color );
      symb.setStroke( stroke );

      final DisplayElement de = DisplayElementFactory.buildLineStringDisplayElement( null, curve, symb );
      de.paint( g, projection, new NullProgressMonitor() );
    }
    catch( final Exception e1 )
    {
      e1.printStackTrace();
    }
  }
}