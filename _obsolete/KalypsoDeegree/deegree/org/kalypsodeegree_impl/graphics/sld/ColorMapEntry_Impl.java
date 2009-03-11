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

import org.kalypsodeegree.graphics.sld.ColorMapEntry;

/**
 * @author N. Peiler
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ColorMapEntry_Impl implements ColorMapEntry
{
  private Color m_color;

  private double m_opacity;

  private double m_quantity;

  private String m_label;

  public ColorMapEntry_Impl( final Color color, final double opacity, final double quantity, final String label )
  {
    m_color = color;
    m_opacity = opacity;
    m_quantity = quantity;
    m_label = label;
  }

  public Color getColor( )
  {
    return m_color;
  }

  public void setColor( final Color color )
  {
    m_color = color;
  }

  public String getLabel( )
  {
    return m_label;
  }

  public void setLabel( final String label )
  {
    m_label = label;
  }

  public double getOpacity( )
  {
    return m_opacity;
  }

  public void setOpacity( final double opacity )
  {
    m_opacity = opacity;
  }

  public double getQuantity( )
  {
    return m_quantity;
  }

  public void setQuantity( final double quantity )
  {
    m_quantity = quantity;
  }

  public String exportAsXML( )
  {
    final StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<ColorMapEntry" );
    sb.append( " label=\"" ).append( getLabel() ).append( "\"" );
    sb.append( " color=\"" ).append( StyleFactory.getColorAsHex( getColor() ) ).append( "\"" );
    sb.append( " opacity=\"" ).append( getOpacity() ).append( "\"" );
    sb.append( " quantity=\"" ).append( getQuantity() ).append( "\"" );
    sb.append( "/>\n" );
    return sb.toString();
  }

  @Override
  public ColorMapEntry clone( )
  {
    return new ColorMapEntry_Impl( m_color, m_opacity, m_quantity, m_label );
  }

}