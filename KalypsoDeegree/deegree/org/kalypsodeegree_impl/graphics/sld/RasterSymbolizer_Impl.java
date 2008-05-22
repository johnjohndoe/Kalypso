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
package org.kalypsodeegree_impl.graphics.sld;

import java.awt.Color;
import java.util.Arrays;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.LineAttributes;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.java.awt.ColorUtilities;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.xml.Marshallable;

/**
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version $Revision$ $Date$
 */
public class RasterSymbolizer_Impl extends Symbolizer_Impl implements RasterSymbolizer, Marshallable
{
  private TreeMap<Double, ColorMapEntry> m_colorMap = null;

  private transient double[] m_values;

  private Color[] m_colors;

  private double m_min;

  private double m_max;

  public RasterSymbolizer_Impl( final TreeMap<Double, ColorMapEntry> colorMap )
  {
    setColorMap( colorMap );
  }

  public TreeMap<Double, ColorMapEntry> getColorMap( )
  {
    return m_colorMap;
  }

  @SuppressWarnings("unchecked")
  public void setColorMap( final TreeMap<Double, ColorMapEntry> colorMap )
  {
    m_colorMap = colorMap;

    // PERFORMANCE: create doubles and colors as arrays for quick access
    final Entry<Double, ColorMapEntry>[] entries = colorMap.entrySet().toArray( new Map.Entry[colorMap.size()] );
    m_values = new double[entries.length];
    m_colors = new Color[entries.length];
    for( int i = 0; i < entries.length; i++ )
    {
      final Entry<Double, ColorMapEntry> entry = entries[i];
      m_values[i] = entry.getKey();
      final Color color = entry.getValue().getColor();
      final double opacity = entry.getValue().getOpacity();
      m_colors[i] = ColorUtilities.createTransparent( color, opacity );
    }

    if( m_values.length == 0 )
    {
      m_min = Double.MAX_VALUE;
      m_max = Double.MIN_VALUE;
    }
    else
    {
      m_min = m_values[0];
      m_max = m_values[m_values.length - 1];
    }
  }

  public String exportAsXML( )
  {
    final StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<RasterSymbolizer" );

    final UOM uom = getUom();
    if( uom != null )
    {
      sb.append( " uom=\"" + uom.name() + "\">\n" );
    }
    else
      sb.append( ">\n" );

    sb.append( "<ColorMap>\n" );

    if( m_colorMap != null )
    {
      for( final Map.Entry<Double, ColorMapEntry> entry : m_colorMap.entrySet() )
      {
        final ColorMapEntry colorMapEntry = entry.getValue();
        sb.append( colorMapEntry.exportAsXML() );
      }
    }

    sb.append( "</ColorMap>\n" );
    sb.append( "</RasterSymbolizer>\n" );

    return sb.toString();
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.RasterSymbolizer#getColor(double)
   */
  public Color getColor( final double value )
  {
    if( value < m_min )
      return null;

    if( value > m_max )
      return null;

    final int binarySearch = Arrays.binarySearch( m_values, value );
    if( binarySearch >= 0 )
      return m_colors[binarySearch];

    final int index = Math.abs( binarySearch ) - 1;
    if( index == m_colors.length )
      return null;

    // Experimental: set to true to linearly interpolate the color
    // Using colormaps with many entries produces the same result
    final boolean interpolate = false;

    if( interpolate )
    {
      if( index == 0 )
        return m_colors[index];

      final Color lower = m_colors[index - 1];
      final Color upper = m_colors[index];

      return interpolate( lower, upper, m_values[index - 1], m_values[index], value );
    }

    return m_colors[index];
  }

  private Color interpolate( final Color lowerColor, final Color upperColor, final double lowerValue, final double upperValue, final double value )
  {
    final double factor = (value - lowerValue) / (upperValue - lowerValue);
    return ColorUtilities.interpolateLinear( lowerColor, upperColor, factor );
  }

  @Override
  public void paint( final GC gc, final Feature feature )
  {
    final Rectangle clipping = gc.getClipping();

    gc.setForeground( gc.getDevice().getSystemColor( SWT.COLOR_BLACK ) );
    gc.setBackground( gc.getDevice().getSystemColor( SWT.COLOR_WHITE ) );
    gc.setLineAttributes( new LineAttributes( 1 ) );

    /* we draw 2 rects in the colors of the color map and a black rectangle around it */
    final org.eclipse.swt.graphics.Color colorStart = new org.eclipse.swt.graphics.Color( gc.getDevice(), m_colors[0].getRed(), m_colors[0].getGreen(), m_colors[0].getBlue() );
    gc.setBackground( colorStart );
    gc.fillRectangle( clipping.x, clipping.y, clipping.width - 1, clipping.height / 2 );

    final org.eclipse.swt.graphics.Color colorEnd = new org.eclipse.swt.graphics.Color( gc.getDevice(), m_colors[m_colors.length - 1].getRed(), m_colors[m_colors.length - 1].getGreen(), m_colors[m_colors.length - 1].getBlue() );
    gc.setBackground( colorEnd );
    gc.fillRectangle( clipping.x, clipping.height / 2, clipping.width - 1, clipping.height - 1 );

    // the black border
    gc.setForeground( gc.getDevice().getSystemColor( SWT.COLOR_BLACK ) );
    gc.drawRectangle( clipping.x, clipping.y, clipping.width - 1, clipping.height - 1 );

    colorStart.dispose();
    colorEnd.dispose();
  }

}