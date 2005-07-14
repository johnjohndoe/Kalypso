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
package org.kalypso.contribs.java.awt;

import java.awt.Color;

import org.kalypso.contribs.java.util.Arrays;

/**
 * Some useful method dealing with java.awt.Color. See also org.kalypso.contribs.java.util.StringUtilities for some
 * other methods using Colors and Strings.
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
   * @return a new Color which components have been given by Math.random().
   */
  public static Color random( final float alpha )
  {
    return new Color( (float)Math.random(), (float)Math.random(), (float)Math.random(), alpha );
  }

  /**
   * Create a color similar to the given one, but distant of the given distance. The color components from the original
   * color are derived as many times as distance. Thus, if distance is 0, the original color is returned. The same alpha
   * value is kept between the original color and the newly created one.
   * 
   * TODO: ameliorate the algorithm for finding derivate color
   */
  public static Color derivateColor( final Color c, int distance )
  {
    if( distance == 0 )
      return c;

    // get color components
    final int a = c.getAlpha();
    final int[] rgb = {c.getRed(), c.getGreen(), c.getBlue()};

    // test if components are all 0 or all 255
    final int[] range = { 0, 255 };
    for( int i = 0; i < range.length; i++ )
    {
      if( rgb[0] == range[i] && rgb[1] == range[i] && rgb[2] == range[i] )
      {
        rgb[0] = (int)( Math.random() * 255 );
        rgb[1] = (int)( Math.random() * 255 );
        rgb[2] = (int)( Math.random() * 255 );
      }
    }
    
    final int pos = Arrays.indexOfMin( rgb );
    
    // derivate color
    for( int i = 0; i < distance; i++ )
    {
      rgb[pos] += 51;
      
      if( rgb[pos] > 255 )
        rgb[pos] = rgb[pos] - 255;
    }
    
    return new Color( rgb[0], rgb[1], rgb[2], a );
  }
}
