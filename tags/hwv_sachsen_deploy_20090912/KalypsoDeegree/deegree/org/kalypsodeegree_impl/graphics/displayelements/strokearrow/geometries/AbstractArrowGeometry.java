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
package org.kalypsodeegree_impl.graphics.displayelements.strokearrow.geometries;

import java.awt.Graphics2D;
import java.awt.geom.AffineTransform;

import org.apache.commons.lang.NotImplementedException;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.graphics.displayelements.LabelFactory;
import org.kalypsodeegree_impl.graphics.displayelements.strokearrow.StrokeArrowHelper.ARROW_WIDGET;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM;

/**
 * @author kuch
 */
public abstract class AbstractArrowGeometry implements IArrowGeometry
{

  private final Graphics2D m_g2;

  private final GeoTransform m_projection;

  private final GM_Point[] m_points;

  private AffineTransform m_savedAT;

  private final UOM m_uom;

  public AbstractArrowGeometry( final Graphics2D g2, final GeoTransform projection, final GM_Point[] points, final UOM uom )
  {
    m_g2 = g2;
    m_projection = projection;
    m_points = points;
    m_uom = uom;
  }

  protected Graphics2D getGraphic( )
  {
    return m_g2;
  }

  protected GeoTransform getProjection( )
  {
    return m_projection;
  }

  protected GM_Point[] getPoints( )
  {
    return m_points;
  }

  /**
   * @see org.kalypsodeegree_impl.graphics.displayelements.strokearrow.IArrowGeometry#paint()
   */
  public void paint( final Double size )
  {
    // calculate screenpoints
    final int[] p1 = LabelFactory.calcScreenCoordinates( getProjection(), getPoints()[0] );
    final int[] p2 = LabelFactory.calcScreenCoordinates( getProjection(), getPoints()[1] );

    if( p1[0] == p2[0] && p1[1] == p2[1] )
      // point coordinates of p1 and p2 must differ
      return;

    // setTransform
    setAffineTransformation( p1, p2 );

    draw( size.intValue(), m_uom, m_projection );

    resetAffineTransformation();
  }

  protected abstract void draw( int size, UOM uom, final GeoTransform projection );

  private void resetAffineTransformation( )
  {
    getGraphic().setTransform( m_savedAT );

  }

  private void setAffineTransformation( final int[] p1, final int[] p2 )
  {
    m_savedAT = getGraphic().getTransform();
    final AffineTransform transform = new AffineTransform();
    transform.translate( p1[0], p1[1] );
    transform.rotate( getRotation() );
    getGraphic().setTransform( transform );
  }

  public static IArrowGeometry getArrowGeometry( final ARROW_WIDGET widget, final Graphics2D g2, final GeoTransform projection, final GM_Point[] points, final UOM uom )
  {
    switch( widget )
    {
      case eFill:
        return new FillArrowGeometry( g2, projection, points, uom );

      case eOpen:
        return new OpenArrowGeometry( g2, projection, points, uom );

      default:
        throw new NotImplementedException();
    }
  }

  protected double getRotation( )
  {
    final double dx = m_points[0].getPosition().getX() - m_points[1].getPosition().getX();
    final double dy = m_points[0].getPosition().getY() - m_points[1].getPosition().getY();

    double atan = Math.atan( dy / dx );

    if( dx >= 0 )
      return -atan;
    else
      return Math.PI - atan;
  }
}
