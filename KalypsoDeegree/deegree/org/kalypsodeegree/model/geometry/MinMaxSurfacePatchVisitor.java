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
package org.kalypsodeegree.model.geometry;

import java.math.BigDecimal;

/**
 * A surface path visitor which determines the min/max z-value of all coordinates of all patches.
 * 
 * @author Gernot Belger
 */
public class MinMaxSurfacePatchVisitor<P extends GM_SurfacePatch> implements ISurfacePatchVisitor<P>
{
  private double m_min = Double.MAX_VALUE;

  private double m_max = Double.MIN_VALUE;

  /**
   * @see org.kalypsodeegree.model.geometry.ISurfacePatchVisitor#visit(org.kalypsodeegree.model.geometry.GM_SurfacePatch,
   *      double)
   */
  public boolean visit( final P surfacePatch, final double elevationSample )
  {
    final GM_Position[] exteriorRing = surfacePatch.getExteriorRing();
    for( final GM_Position position : exteriorRing )
    {
      final double[] asArray = position.getAsArray();
      // REMARK: ignores positions without z-value
      if( asArray.length > 2 )
      {
        final double z = asArray[2];
        m_min = Math.min( m_min, z );
        m_max = Math.max( m_max, z );
      }
    }

    return true;
  }

  /**
   * @return The minimum value of all encountered z-values.<br>
   *         Double#MAX_VALUE for the empty surface.
   */
  public BigDecimal getMin( )
  {
    return BigDecimal.valueOf( m_min );
  }

  /**
   * @return The minimum value of all encountered z-values.<br>
   *         Double#MIN_VALUE for the empty surface.
   */
  public BigDecimal getMax( )
  {
    return BigDecimal.valueOf( m_max );
  }

}
