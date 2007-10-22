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
package org.kalypsodeegree_impl.graphics.sld;

import java.awt.Color;
import java.util.Arrays;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;

import org.kalypso.contribs.java.awt.ColorUtilities;
import org.kalypsodeegree.graphics.sld.ColorMapEntry;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
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

    sb.append( "<ColorMap>" );

    if( m_colorMap != null )
    {
      for( final Map.Entry<Double, ColorMapEntry> entry : m_colorMap.entrySet() )
      {
        final ColorMapEntry colorMapEntry = entry.getValue();
        sb.append( colorMapEntry.exportAsXML() );
      }
    }

    sb.append( "</ColorMap>" );
    sb.append( "</RasterSymbolizer>" );

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

    final int index = Math.abs( Arrays.binarySearch( m_values, value ) );
    if( index >= 0 && index < m_colors.length )
      return m_colors[index];

    return null;
  }
}