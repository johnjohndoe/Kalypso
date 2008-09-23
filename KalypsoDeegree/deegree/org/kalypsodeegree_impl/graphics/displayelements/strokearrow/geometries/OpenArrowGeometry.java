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

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM;

/**
 * @author Dirk Kuch
 */
public class OpenArrowGeometry extends AbstractArrowGeometry
{

  public OpenArrowGeometry( final Graphics2D g2, final GeoTransform projection, final GM_Point[] points, final UOM uom )
  {
    super( g2, projection, points, uom );
  }

  /**
   * @see org.kalypsodeegree_impl.graphics.displayelements.strokearrow.geometries.AbstractArrowGeometry#draw(int)
   */
  @Override
  protected void draw( final int size, final UOM uom, final GeoTransform projection )
  {
    int size_2 = 0; // length
    int size_3 = 0; // width

    switch( uom )
    {
      case pixel:
        size_2 = size / 2;
        size_3 = size / 4;

        break;

      case meter:

        final GM_Envelope sourceRect = projection.getSourceRect();
        final double sizeFromNull = projection.getDestX( sourceRect.getMin().getX() );
        final double sizeFromMeters = projection.getDestX( sourceRect.getMin().getX() + size );
        final double lengthInMeters = Math.abs( sizeFromMeters - sizeFromNull );
        size_2 = (int) lengthInMeters / 2;
        size_3 = (int) lengthInMeters / 4;

        break;

      case foot:

        throw new UnsupportedOperationException();
    }

    final int[] a = new int[] { -size_2, +size_3 };
    final int[] b = new int[] { -size_2, -size_3 };
    final int[] c = new int[] { 0, 0 };

    final Graphics2D graphic = getGraphic();

    graphic.drawLine( c[0], c[1], a[0], a[1] );
    graphic.drawLine( c[0], c[1], b[0], b[1] );

  }

}
