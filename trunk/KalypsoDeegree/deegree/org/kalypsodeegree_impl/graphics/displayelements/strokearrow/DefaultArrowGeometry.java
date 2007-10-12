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
package org.kalypsodeegree_impl.graphics.displayelements.strokearrow;

import java.awt.Graphics2D;

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author kuch
 */
public class DefaultArrowGeometry extends AbstractArrowGeometry
{

  public DefaultArrowGeometry( Graphics2D g2, GeoTransform projection, GM_Point[] points )
  {
    super( g2, projection, points );
  }

  /**
   * @see org.kalypsodeegree_impl.graphics.displayelements.strokearrow.AbstractArrowGeometry#draw(java.lang.Double)
   */
  @Override
  protected void draw( int size )
  {
    // draw triangle
    int size_4 = size / 4;

    int[] a = new int[] { -size, +size_4 };
    int[] b = new int[] { -size, -size_4 };
    int[] c = new int[] { 0, 0 };

    int[] x = new int[] { a[0], b[0], c[0] };
    int[] y = new int[] { a[1], b[1], c[1] };

    getGraphic().drawPolygon( x, y, 3 );
  }
}
