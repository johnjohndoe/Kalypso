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

/**
 * A triangulated surface.
 * 
 * @author Gernot Belger
 */
public interface GM_TriangulatedSurface extends GM_Surface<GM_Triangle>
{
  /**
   * Returns the value of the triangulated at the given position.
   * <p>
   * The value is obtained by finding the first triangle which covers the given position and calculating the z-value of
   * the position of the linear-equation defined by the (corner-coordiantes of the) triangle.
   * </p>
   * 
   * @param positionMust
   *            be in the same coordinate system as the tirangulated surface.
   * @return The linearly interpolated z-value of the first triangle covering the position. <code>Double.NaN</code> if
   *         no covering triangle was found.
   */
  public double getValue( final GM_Point location );

  /**
   * Returns the value of the triangulated at the given position.
   * <p>
   * The value is obtained by finding the first triangle which covers the given position and calculating the z-value of
   * the position of the linear-equation defined by the (corner-coordiantes of the) triangle.
   * </p>
   * 
   * @param positionMust
   *            be in the same coordinate system as the tirangulated surface.
   * @return The linearly interpolated z-value of the first triangle covering the position. <code>Double.NaN</code> if
   *         no covering triangle was found.
   */
  public double getValue( GM_Position pos );

  public GM_Triangle getTriangle( GM_Position pos );
}
