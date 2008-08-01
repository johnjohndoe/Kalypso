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

package org.kalypsodeegree.model.coverage;

/**
 * The <tt>GridExtentDescription</tt> that extends the shared <tt>DomainSetExtentDescription</tt>. It adds
 * three/four fields to the <tt>DomainSetExtentDescription</tt>
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
public interface GridExtentDescription extends DomainSetExtentDescription
{

  /**
   * identifies each axis of the grid
   */
  GridAxisDescription getGridAxisDescription();

  /**
   * GridSpacing is required for rectified grids, but undefined for other grids. It provides the ground resolution
   * (pixel size or post spacing) along each dimension of the grid, expressed in the units of the (rectified) grid�s
   * Coordinate Reference System
   */
  GridSpacing getGridSpacing();

  /**
   * Either Grid or RectifiedGrid is required. Both elements list the upper and lower bounds of the grid coordinates
   * along each of the grid axes. (These are integer pixel or post coordinates, expressed in the grid�s internal
   * coordinate reference system. The lower bounds are commonly defined as zero.)
   */
  Grid getGrid();

  /**
   * Either Grid or RectifiedGrid is required. Both elements list the upper and lower bounds of the grid coordinates
   * along each of the grid axes. (These are integer pixel or post coordinates, expressed in the grid�s internal
   * coordinate reference system. The lower bounds are commonly defined as zero.)
   */
  RectifiedGrid getRectifiedGrid();

}