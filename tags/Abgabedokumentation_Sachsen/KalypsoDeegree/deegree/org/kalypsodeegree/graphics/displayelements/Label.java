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
package org.kalypsodeegree.graphics.displayelements;

import java.awt.Graphics2D;

/**
 * This is a label with style information and screen coordinates, ready to be
 * rendered to a view.
 * <p>
 * 
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public interface Label
{

  /**
   * Renders the label (including halo and other style information) to the
   * submitted <tt>Graphics2D</tt> context.
   * <p>
   * 
   * @param g
   *          <tt>Graphics2D</tt> context to be used
   */
  public void paint( Graphics2D g );

  /**
   * Renders the label's boundaries to the submitted <tt>Graphics2D</tt>
   * context. Immensely useful for testing.
   * <p>
   * 
   * @param g
   *          <tt>Graphics2D</tt> context to be used
   */
  public void paintBoundaries( Graphics2D g );

  /**
   * Determines if the label intersects with another label.
   * <p>
   * 
   * @param that
   *          label to test
   * @return true if the labels intersect
   */
  public boolean intersects( Label that );

  /**
   * Returns the x-coordinate of the label.
   * <p>
   * 
   * @return
   */
  public int getX();

  /**
   * Returns the y-coordinate of the label.
   * <p>
   * 
   * @return
   */
  public int getY();

  /**
   * Returns the rightmost x-coordinate of the label's bounding box.
   * <p>
   * 
   * @return
   */
  public int getMaxX();

  /**
   * Returns the lowermost y-coordinate of the label's bounding box.
   * <p>
   * 
   * @return
   */
  public int getMaxY();

  /**
   * Returns the leftmost x-coordinate of the label's bounding box.
   * <p>
   * 
   * @return
   */
  public int getMinX();

  /**
   * Returns the uppermost x-coordinate of the label's bounding box.
   * <p>
   * 
   * @return
   */
  public int getMinY();
}