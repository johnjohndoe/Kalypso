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

import java.awt.Graphics;
import java.awt.Point;

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * This class is a geometry builder for a single point.
 * 
 * @author Gernot Belger
 */
public class PointGeometryBuilder implements IGeometryBuilder
{
  private final CS_CoordinateSystem m_crs;

  private GM_Point m_point;

  private GM_Point m_result;

  /**
   * @param targetCrs
   *            The target coordinate system.
   */
  public PointGeometryBuilder( final CS_CoordinateSystem targetCrs )
  {
    m_crs = targetCrs;
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.IGeometryBuilder#addPoint(org.kalypsodeegree.model.geometry.GM_Position)
   */
  public GM_Object addPoint( final GM_Point p ) throws Exception
  {
    m_point = p;

    return finish();
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.IGeometryBuilder#finish()
   */
  public GM_Object finish( ) throws Exception
  {
    if( m_result != null )
      return m_result;

    final GeoTransformer transformer = new GeoTransformer( m_crs );

    final GM_Point transformedPoint = (GM_Point) transformer.transform( m_point );

    m_result = GeometryFactory.createGM_Point( transformedPoint.getPosition(), m_crs );
    return m_result;
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.IGeometryBuilder#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform)
   */
  public void paint( final Graphics g, final GeoTransform projection, final Point currentPoint )
  {
    if( m_point == null )
      drawHandles( g, new int[] { (int) currentPoint.getX() }, new int[] { (int) currentPoint.getY() } );
    else if( currentPoint != null )
    {
      final int x = (int) projection.getDestX( m_point.getX() );
      final int y = (int) projection.getDestY( m_point.getY() );

      drawHandles( g, new int[] { x }, new int[] { y } );
    }
  }

  private void drawHandles( final Graphics g, final int[] x, final int[] y )
  {
    final int sizeOuter = 6;
    for( int i = 0; i < y.length; i++ )
      g.drawRect( x[i] - sizeOuter / 2, y[i] - sizeOuter / 2, sizeOuter, sizeOuter );
  }
}