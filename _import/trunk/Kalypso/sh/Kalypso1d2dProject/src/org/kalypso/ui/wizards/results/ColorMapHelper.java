/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.ui.wizards.results;

import java.awt.Color;

/**
 * @author Thomas Jung
 * 
 */
public class ColorMapHelper
{

  /**
   * returns the interpolated color of a colormap defined by start and end color.F
   * 
   * @param currentClass
   *            current class
   * @param numOfClasses
   *            number of all classes in which the colormap is divided.
   */
  public static Color interpolateColor( final Color minColor, final Color maxColor, final int currentClass, final int numOfClasses )
  {
    // interpolate color
    final float[] minhsb = Color.RGBtoHSB( minColor.getRed(), minColor.getGreen(), minColor.getBlue(), null );
    final float[] maxhsb = Color.RGBtoHSB( maxColor.getRed(), maxColor.getGreen(), maxColor.getBlue(), null );

    final float minHue = minhsb[0];
    final float maxHue = maxhsb[0];

    final float minSat = minhsb[1];
    final float maxSat = maxhsb[1];

    final float minBri = minhsb[2];
    final float maxBri = maxhsb[2];

    final double Hue = minHue + (currentClass * (maxHue - minHue) / (numOfClasses - 1));
    final double Sat = minSat + (currentClass * (maxSat - minSat) / (numOfClasses - 1));
    final double Bri = minBri + (currentClass * (maxBri - minBri) / (numOfClasses - 1));

    final Color hsbColor = Color.getHSBColor( (float) Hue, (float) Sat, (float) Bri );
    final Color rgbColor = new Color( hsbColor.getRed(), hsbColor.getGreen(), hsbColor.getBlue() );

    return rgbColor;
  }

}
