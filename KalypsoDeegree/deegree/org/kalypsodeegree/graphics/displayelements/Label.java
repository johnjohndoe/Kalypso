/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

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

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 ---------------------------------------------------------------------------*/
package org.deegree.graphics.displayelements;

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