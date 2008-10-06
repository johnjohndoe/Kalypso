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
package org.kalypsodeegree_impl.graphics.displayelements.strokearrow.geometries;

import java.awt.Graphics2D;

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.graphics.sld.Symbolizer_Impl.UOM;

/**
 * @author kuch
 */
public class FillArrowGeometry extends AbstractArrowGeometry
{

  public FillArrowGeometry( final Graphics2D g2, final GeoTransform projection, final GM_Point[] points, final UOM uom )
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
    int size_3 = 0; // widths

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

    final int[] x = new int[] { 0, -size_2, -size_2, 0 };
    final int[] y = new int[] { 0, -size_3, +size_3, 0 };

    final Graphics2D graphic = getGraphic();

    graphic.drawPolygon( x, y, 4 );
    graphic.fillPolygon( x, y, 4 );
  }

}
