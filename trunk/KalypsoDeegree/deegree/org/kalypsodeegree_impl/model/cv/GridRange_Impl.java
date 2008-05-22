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
package org.kalypsodeegree_impl.model.cv;

import org.kalypsodeegree.model.coverage.GridRange;

/**
 * Implements the range of valid coordinates for each dimension of the coverage.
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
public class GridRange_Impl implements GridRange
{
  private double[] m_low = null;

  private double[] m_high = null;

  public GridRange_Impl( final double[] low, final double[] high )
  {
    this.m_low = low;
    this.m_high = high;
  }

  /**
   * The valid maximum exclusive grid coordinate. The sequence contains a maximum value for each dimension of the grid
   * coverage.
   */
  public double[] getHigh( )
  {
    return m_high;
  }

  /**
   * The valid minimum inclusive grid coordinate. The sequence contains a minimum value for each dimension of the grid
   * coverage. The lowest valid grid coordinate is zero.
   */
  public double[] getLow( )
  {
    return m_low;
  }

  @Override
  public Object clone( )
  {
    return new GridRange_Impl( m_low.clone(), m_high.clone() );
  }
}