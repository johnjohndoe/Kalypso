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

package org.kalypsodeegree_impl.graphics.transformation;

import org.kalypso.transformation.GeoTransformer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * the class <code>WorldToScreenTransform</code> implements <code>GeoTransformInterface</code> and defines a
 * transformation to a linear coordinat system with its orgin on top/left. this can be used for realising a screen
 * mapping of geometries.
 *
 * @author Andreas Poth poth@lat-lon.de
 * @version 28.12.2000
 */

public class WorldToScreenTransform implements GeoTransform
{
  private static final int EARTH_RADIUS_M = 6378137;

  private double m_qx = 0;

  private double m_qy = 0;

  private GM_Envelope m_sourceRect = null;

  private GM_Envelope m_destRect = null;

  /**
   * Initialises the transformation rectangles with unique values (origin 0/0; widht 1; height 1)
   */
  public WorldToScreenTransform( )
  {
    setSourceRect( 0, 0, 1, 1, null );
    setDestRect( 0, 0, 1, 1, null );
  }

  /**
   * Initialises the transformation rectangle using the submitted source- and destination rectangle.
   *
   * @param sourceRect
   *          is the boundary of the source geometry.
   * @param destRect
   *          is the boundary of the destination rectangle (for example a region on the screen)
   */
  public WorldToScreenTransform( final GM_Envelope sourceRect, final GM_Envelope destRect )
  {
    setSourceRect( sourceRect );
    setDestRect( destRect );
  }

  /**
   * @param sourceXMin
   *          minimum x-coordinate (source system) of the map.
   * @param sourceYMin
   *          minimum y-coordinate (source system) of the map.
   * @param sourceXMax
   *          maximum x-coordinate (source system) of the map.
   * @param sourceYMax
   *          maximum y-coordinate (source system) of the map.
   * @param sourceCoordinateSystem
   *          The coordinate system (source system) of the map.
   * @param destXMin
   *          minimum x-coordinate (destination system) of the map.
   * @param destYMin
   *          minimum y-coordinate (destination system) of the map.
   * @param destXMax
   *          maximum x-coordinate (destination system) of the map.
   * @param destYMax
   *          maximum y-coordinate (destination system) of the map.
   * @param destCoordinateSystem
   *          The coordinate system (destination system) of the map.
   */
  public WorldToScreenTransform( final double sourceXMin, final double sourceYMin, final double sourceXMax, final double sourceYMax, final String sourceCoordinateSystem, final double destXMin, final double destYMin, final double destXMax, final double destYMax, final String destCoordinateSystem )
  {
    setSourceRect( sourceXMin, sourceYMin, sourceXMax, sourceYMax, sourceCoordinateSystem );
    setDestRect( destXMin, destYMin, destXMax, destYMax, destCoordinateSystem );
  }

  /**
   * sets the source rectangle
   *
   * @param rect
   *          is the boundary of the source geometry.
   */
  public void setSourceRect( final GM_Envelope rect )
  {
    if( rect == null )
      throw new NullPointerException();

    m_sourceRect = rect;

    if( (m_sourceRect != null) && (m_destRect != null) )
    {
      calculateQX();
      calculateQY();
    }
  }

  /**
   * sets the source rectangle
   *
   * @param xMin
   *          minimum x-coordinate (source system) of the map.
   * @param yMin
   *          minimum y-coordinate (source system) of the map.
   * @param xMax
   *          maximum x-coordinate (source system) of the map.
   * @param yMax
   *          maximum y-coordinate (source system) of the map.
   * @param sourceCoordinateSystem
   *          The coordinate system (source system) of the map.
   */
  public void setSourceRect( double xMin, double yMin, double xMax, double yMax, final String sourceCoordinateSystem )
  {
    double dum = 0;

    if( xMin > xMax )
    {
      dum = xMax;
      xMax = xMin;
      xMin = dum;
    }

    if( yMin > yMax )
    {
      dum = yMax;
      yMax = yMin;
      yMin = dum;
    }

    setSourceRect( GeometryFactory.createGM_Envelope( xMin, yMin, xMax, yMax, sourceCoordinateSystem ) );
  }

  public GM_Envelope getSourceRect( )
  {
    return m_sourceRect;
  }

  /**
   * sets the destination rectangle.
   *
   * @param rect
   *          is the boundary of the destination rectangle (for example a region on the screen)
   */
  public void setDestRect( final GM_Envelope rect )
  {
    if( rect == null )
      throw new NullPointerException();

    m_destRect = rect;
    if( m_sourceRect != null && m_destRect != null )
    {
      calculateQX();
      calculateQY();
    }
  }

  /**
   * sets the destination rectangle
   *
   * @param xMin
   *          minimum x-coordinate (destination system) of the map.
   * @param yMin
   *          minimum y-coordinate (destination system) of the map.
   * @param xMax
   *          maximum x-coordinate (destination system) of the map.
   * @param yMax
   *          maximum y-coordinate (destination system) of the map.
   * @param destCoordinateSystem
   *          The coordinate system (destination system) of the map.
   */
  public void setDestRect( double xMin, double yMin, double xMax, double yMax, final String destCoordinateSystem )
  {
    double dum = 0;

    if( xMin > xMax )
    {
      dum = xMax;
      xMax = xMin;
      xMin = dum;
    }

    if( yMin > yMax )
    {
      dum = yMax;
      yMax = yMin;
      yMin = dum;
    }

    setDestRect( GeometryFactory.createGM_Envelope( xMin, yMin, xMax, yMax, destCoordinateSystem ) );
  }

  /**
   * executes a coordinat transformation for the submitted x-coordinate of the source coordinat system.
   *
   * @param xsource
   *          , x-coordinate of a point in the source coordinate system.
   * @return the x-coordinate of the submitted value in the destination coordinate system.
   */
  public double getDestX( final double xsource )
  {
    return m_destRect.getMin().getX() + (xsource - m_sourceRect.getMin().getX()) * m_qx;
  }

  /**
   * executes a coordinat transformation for the submitted y-coordinate of the source coordinat system.
   *
   * @param ysource
   *          , y-coordinate of a point in the source coordinate system.
   * @return the y-coordinate of the submitted value in the destination coordinate system.
   */
  public double getDestY( final double ysource )
  {
    return m_destRect.getMin().getY() + m_destRect.getHeight() - (ysource - m_sourceRect.getMin().getY()) * m_qy;
  }

  /**
   * executes a coordinate transformation for the submitted point of the source coordinat system.
   *
   * @param point
   *          in the source coordinate system.
   * @return the location of the submitted point in the destination coordinate system.
   */
  public GM_Position getDestPoint( final GM_Position point )
  {
    final double x = getDestX( point.getX() );
    final double y = getDestY( point.getY() );
    return GeometryFactory.createGM_Position( x, y );
  }

  /**
   * executes a coordinate transformation for the submitted x-coordinate of the destination coordinate system.
   *
   * @param xdest
   *          , x-coordinate of a point in the destination coordinate system.
   * @return the x-coordinate of the submitted value in the source coordinate system.
   */
  public double getSourceX( final double xdest )
  {
    return (xdest - m_destRect.getMin().getX()) / m_qx + m_sourceRect.getMin().getX();
  }

  /**
   * executes a coordinat transformation for the submitted y-coordinate of the destination coordinate system.
   *
   * @param ydest
   *          , y-coordinate of a point in the destination coordinate system.
   * @return the y-coordinate of the submitted value in the source coordinate system.
   */
  public double getSourceY( final double ydest )
  {
    return (m_destRect.getHeight() - (ydest - m_destRect.getMin().getY())) / m_qy + m_sourceRect.getMin().getY();

  }

  /**
   * executes a coordinate transformation for the submitted point of the destination coordinate system.
   *
   * @param point
   *          in the destination coordinate system.
   * @return the location of the submitted point in the source coordinate system.
   */
  public GM_Position getSourcePoint( final GM_Position point )
  {
    final double x = getSourceX( point.getX() );
    final double y = getSourceY( point.getY() );
    return GeometryFactory.createGM_Position( x, y );
  }

  public double getDestWidth( )
  {
    return m_destRect.getWidth();
  }

  public double getDestHeight( )
  {
    return m_destRect.getHeight();
  }

  /**
   * calculates the relation between the width of the destination and the source coordinate system.
   */
  private void calculateQX( )
  {
    m_qx = m_destRect.getWidth() / m_sourceRect.getWidth();
  }

  /**
   * calculates the relation between the height of the destination and the source coordinate system.
   */
  private void calculateQY( )
  {
    m_qy = m_destRect.getHeight() / m_sourceRect.getHeight();
  }

  /**
   * Calculates the current scale (denominator) as defined in the OGC SLD 1.0.0 specification.
   *
   * @return scale of the map
   */
  public double getScale( )
  {
    final GM_Envelope box = getWgs84SorceRect();
    if( box == null )
      return 0.0;

    // As long as we do not know the real pixel size (dpi) of the current graphics context, we
    // assume quadratic pixels of 0.28 mm size.

    final double dLon = box.getMax().getX() - box.getMin().getX(); // Map-x-Distance in deegrees
    // final double dLat = box.getMax().getY() - box.getMin().getY(); // Map-y-Distance in deegrees

    final double mx = Math.toRadians( dLon ) * EARTH_RADIUS_M; // Map-x-Distance in Meters
    // final double my = Math.toRadians( dLat ) * EARTH_RADIUS_M; // Map-y-Distance in Meters

    final double scalex = mx / getDestWidth() / 0.00028;
    // final double scaley = my / getDestHeight() / 0.00028;

    return scalex;
  }

  private GM_Envelope getWgs84SorceRect( )
  {
    if( m_sourceRect == null )
      return null;

    final String crs = m_sourceRect.getCoordinateSystem();
    if( crs == null )
      return null;

    if( crs.equalsIgnoreCase( "EPSG:4326" ) ) //$NON-NLS-1$
      return m_sourceRect;

    try
    {
      // transform the bounding box of the request to EPSG:4326
      final GeoTransformer transformer = new GeoTransformer( "EPSG:4326" ); //$NON-NLS-1$
      return transformer.transformEnvelope( m_sourceRect, crs );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }
}