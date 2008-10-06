/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ui.editor.sldEditor;

import java.awt.Color;
import java.math.BigDecimal;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

/**
 * @author Thomas Jung
 */
public class SldHelper
{

  /**
   * returns the interpolated color of a colormap defined by start and end color.
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

  public static double interpolate( final double min, final double max, final int currentClass, final int numOfClasses )
  {
    if( currentClass == 0 || numOfClasses == 1 )
      return min;

    return min + (currentClass * (max - min) / (numOfClasses - 1));
  }

  /**
   * checks the user typed string for the double value
   * 
   * @param comp
   *            composite of the text field
   * @param text
   *            the text field
   */
  public static BigDecimal checkDoubleTextValue( final Composite comp, final Text text, final Pattern pattern )
  {
    String tempText = text.getText();

    final Matcher m = pattern.matcher( tempText );

    if( !m.matches() )
    {
      text.setBackground( comp.getDisplay().getSystemColor( SWT.COLOR_RED ) );
    }
    else
    {
      text.setBackground( comp.getDisplay().getSystemColor( SWT.COLOR_WHITE ) );
      tempText = tempText.replaceAll( ",", "." );

      final BigDecimal db = new BigDecimal( tempText );
      text.setText( db.toString() );

      return db;
    }
    return null;
  }

  /**
   * checks the user typed a string for a positive double value, if it is negative the value is set to 0.
   * 
   * @param comp
   *            composite of the text field
   * @param text
   *            the text field
   */
  public static BigDecimal checkPositiveDoubleTextValue( final Composite comp, final Text text, final Pattern pattern )
  {
    String tempText = text.getText();

    final Matcher m = pattern.matcher( tempText );

    if( !m.matches() )
    {
      text.setBackground( comp.getDisplay().getSystemColor( SWT.COLOR_RED ) );
    }
    else
    {
      text.setBackground( comp.getDisplay().getSystemColor( SWT.COLOR_WHITE ) );
      tempText = tempText.replaceAll( ",", "." );

      BigDecimal db = new BigDecimal( tempText );
      if( db.doubleValue() > 0 )
      {
        text.setText( db.toString() );
      }
      else
      {
        db = new BigDecimal( "0.0" );
        text.setText( db.toString() );
      }

      return db;
    }
    return null;
  }

}
