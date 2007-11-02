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
package org.kalypso.ui.editor.sldEditor;

import java.awt.Color;
import java.math.BigDecimal;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.LineColorMapEntry;
import org.kalypsodeegree.graphics.sld.ParameterValueType;
import org.kalypsodeegree.graphics.sld.PolygonColorMapEntry;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.sld.SurfaceLineSymbolizer;
import org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer;
import org.kalypsodeegree_impl.graphics.sld.LineColorMap;
import org.kalypsodeegree_impl.graphics.sld.LineColorMapEntry_Impl;
import org.kalypsodeegree_impl.graphics.sld.LineColorMap_Impl;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMap;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMapEntry_Impl;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMap_Impl;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;

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
    return min + (currentClass * (max - min) / (numOfClasses - 1));
  }

  private static void configurePolygonSymbolizer( final SurfacePolygonSymbolizer symbolizer, final BigDecimal minValue, final BigDecimal maxValue ) throws FilterEvaluationException
  {
    final PolygonColorMap templateColorMap = symbolizer.getColorMap();
    final PolygonColorMap newColorMap = new PolygonColorMap_Impl();

    // retrieve stuff from template-entries
    final PolygonColorMapEntry fromEntry = templateColorMap.findEntry( "from", null );
    final PolygonColorMapEntry toEntry = templateColorMap.findEntry( "to", null );

    // Fill
    final Color fromPolygonColor = fromEntry.getFill().getFill( null );
    final Color toPolygonColor = toEntry.getFill().getFill( null );
    final double polygonOpacity = fromEntry.getFill().getOpacity( null );

    // Stroke
    final Color fromLineColor = fromEntry.getStroke().getStroke( null );
    final Color toLineColor = toEntry.getStroke().getStroke( null );
    final double lineOpacity = fromEntry.getStroke().getOpacity( null );

    // step width
    final double stepWidth = fromEntry.getTo( null );

    // scale of the step width
    final BigDecimal setScale = new BigDecimal( fromEntry.getFrom( null ) ).setScale( 0, BigDecimal.ROUND_FLOOR );
    final int stepWidthScale = setScale.intValue();

    // get rounded values below min and above max (rounded by first decimal)
    // as a first try we will generate isareas by using class steps of 0.1
    // later, the classes will be created by using user defined class steps.
    // for that we fill an array of calculated (later user defined values) from max to min
    final BigDecimal minDecimal = minValue.setScale( 1, BigDecimal.ROUND_FLOOR );
    final BigDecimal maxDecimal = maxValue.setScale( 1, BigDecimal.ROUND_CEILING );

    final BigDecimal polygonStepWidth = new BigDecimal( stepWidth ).setScale( stepWidthScale, BigDecimal.ROUND_FLOOR );
    final int numOfClasses = (maxDecimal.subtract( minDecimal ).divide( polygonStepWidth )).intValue();

    for( int currentClass = 0; currentClass < numOfClasses; currentClass++ )
    {
      final double fromValue = minDecimal.doubleValue() + currentClass * polygonStepWidth.doubleValue();
      final double toValue = minDecimal.doubleValue() + (currentClass + 1) * polygonStepWidth.doubleValue();

      // Stroke
      Color lineColor;
      if( fromLineColor == toLineColor )
        lineColor = fromLineColor;
      else
        lineColor = interpolateColor( fromLineColor, toLineColor, currentClass, numOfClasses );

      // Fill
      final Color polygonColor = interpolateColor( fromPolygonColor, toPolygonColor, currentClass, numOfClasses );
      lineColor = polygonColor;

      final Stroke stroke = StyleFactory.createStroke( lineColor, lineOpacity, 1 );

      final Fill fill = StyleFactory.createFill( polygonColor, polygonOpacity );

      final ParameterValueType label = StyleFactory.createParameterValueType( "Isofl‰che " + currentClass );
      final ParameterValueType from = StyleFactory.createParameterValueType( fromValue );
      final ParameterValueType to = StyleFactory.createParameterValueType( toValue );

      final PolygonColorMapEntry colorMapEntry = new PolygonColorMapEntry_Impl( fill, stroke, label, from, to );
      newColorMap.addColorMapClass( colorMapEntry );
    }

    symbolizer.setColorMap( newColorMap );
  }

  /**
   * sets the parameters for the colormap of an isoline
   */
  private static void configureLineSymbolizer( final SurfaceLineSymbolizer symbolizer, final BigDecimal minValue, final BigDecimal maxValue ) throws FilterEvaluationException
  {
    final LineColorMap templateColorMap = symbolizer.getColorMap();
    final LineColorMap newColorMap = new LineColorMap_Impl();

    // retrieve stuff from template-entries
    final LineColorMapEntry fromEntry = templateColorMap.findEntry( "from", null );
    final LineColorMapEntry toEntry = templateColorMap.findEntry( "to", null );
    final LineColorMapEntry fatEntry = templateColorMap.findEntry( "fat", null );

    final Color fromColor = fromEntry.getStroke().getStroke( null );
    final Color toColor = toEntry.getStroke().getStroke( null );
    final double opacity = fromEntry.getStroke().getOpacity( null );

    final double normalWidth = fromEntry.getStroke().getWidth( null );
    final double fatWidth = fatEntry.getStroke().getWidth( null );

    // defines which isolines are drawn with a fat line
    final double fatValue = fatEntry.getQuantity( null );
    // TODO: get setep / scale from quantity
    // get rounded values below min and above max (rounded by first decimal)
    // as a first try we will generate isolines using class steps of 0.1
    // later, the classes will be done by using user defined class steps.
    // for that we fill an array of calculated (later user defined values) from max to min
    final BigDecimal minDecimal = minValue.setScale( 1, BigDecimal.ROUND_FLOOR );
    final BigDecimal maxDecimal = maxValue.setScale( 1, BigDecimal.ROUND_CEILING );

    final BigDecimal stepWidth = new BigDecimal( 0.1 ).setScale( 1, BigDecimal.ROUND_HALF_UP );
    final int numOfClasses = (maxDecimal.subtract( minDecimal ).divide( stepWidth )).intValue() + 1;

    for( int currentClass = 0; currentClass < numOfClasses; currentClass++ )
    {
      final double currentValue = minDecimal.doubleValue() + currentClass * stepWidth.doubleValue();

      Color lineColor;
      if( fromColor == toColor )
        lineColor = fromColor;
      else
        lineColor = interpolateColor( fromColor, toColor, currentClass, numOfClasses );

      final double strokeWidth;
      if( currentValue % fatValue == 0 )
        strokeWidth = fatWidth;
      else
        strokeWidth = normalWidth;

      final Stroke stroke = StyleFactory.createStroke( lineColor, strokeWidth, opacity );

      final ParameterValueType label = StyleFactory.createParameterValueType( "Isolinie " + currentClass );
      final ParameterValueType quantity = StyleFactory.createParameterValueType( currentValue );

      final LineColorMapEntry colorMapEntry = new LineColorMapEntry_Impl( stroke, label, quantity );
      newColorMap.addColorMapClass( colorMapEntry );
    }

    symbolizer.setColorMap( newColorMap );
  }

  /**
   * checks the user typed string for the double value
   * 
   * @param comp
   *            composite of the text field
   * @param text
   *            the text field
   */
  public static BigDecimal checkDoubleTextValue( final Composite comp, final Text text, Pattern pattern )
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
  public static BigDecimal checkPositiveDoubleTextValue( final Composite comp, final Text text, Pattern pattern )
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
