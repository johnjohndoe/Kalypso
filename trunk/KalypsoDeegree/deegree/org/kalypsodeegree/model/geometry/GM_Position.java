/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 
 history:
  
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
     
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree.model.geometry;

/**
 * A sequence of decimals numbers which when written on a width are a sequence
 * of coordinate positions. The width is derived from the CRS or coordinate
 * dimension of the container.
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @version 5.6.2001
 * @author Andreas Poth
 *         <p>
 */
public interface GM_Position
{
  /**
   * returns the x-value of the point
   */
  public double getX();

  /**
   * returns the y-value of the point
   */
  public double getY();

  /**
   * returns the z-value of the point
   */
  public double getZ();

  /**
   * returns the x- and y-value of the point as a two dimensional array the
   * first field contains the x- the second field the y-value.
   */
  public double[] getAsArray();

  /**
   * translates the coordinates of the position. the first coordinate of the
   * position will be translated by the first field of <tt>d</tt> the second
   * coordinate by the second field of <tt>d</tt> and so on...
   */
  public void translate( double[] d );

  public double getDistance( GM_Position position );
}