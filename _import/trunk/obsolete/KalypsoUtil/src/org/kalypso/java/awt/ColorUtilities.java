/*--------------- Kalypso-Header --------------------------------------------------------------------

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
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.java.awt;

import java.awt.Color;

/**
 * Some useful method dealing with java.awt.Color. See also org.kalypso.java.util.StringUtilities for some other methods
 * using Colors and Strings.
 * 
 * @see org.kalypso.java.util.StringUtilities
 * 
 * @author schlienger
 */
public class ColorUtilities
{
  /**
   * creates a returns the complementary Color of org. Each component of the color is substracted to 255 and set as
   * component of the new color.
   * 
   * @param org
   * 
   * @return new Color
   */
  public static Color createComplementary( final Color org )
  {
    final Color cmp = new Color( 255 - org.getRed(), 255 - org.getGreen(), 255 - org.getBlue() );

    return cmp;
  }

  /**
   * Create a random color. Alpha defaults to 1.
   * 
   * @return a new Color which components have been given by Math.random().
   */
  public static Color random()
  {
    return new Color( (float)Math.random(), (float)Math.random(), (float)Math.random() );
  }

  /**
   * Create a random color with the given alpha value
   * 
   * @param alpha
   * 
   * @return a new Color which components have been given by Math.random().
   */
  public static Color random( final float alpha )
  {
    return new Color( (float)Math.random(), (float)Math.random(), (float)Math.random(), alpha );
  }
}
