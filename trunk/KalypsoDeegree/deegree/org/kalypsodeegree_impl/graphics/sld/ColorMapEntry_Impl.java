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

import net.opengis.sld.ObjectFactory;

import org.kalypsodeegree.graphics.sld.ColorMapEntry;

/**
 * @author N. Peiler
 */
public class ColorMapEntry_Impl implements ColorMapEntry
{
  private Color m_color = null;

  private double m_opacity = 1;

  private double m_quantity = -9999;

  private String m_label = " ";

  private static final ObjectFactory OF = new ObjectFactory();

  /**
   * @param color
   * @param opacity
   * @param quantity
   * @param label
   */
  public ColorMapEntry_Impl( Color color, double opacity, double quantity, String label )
  {
    super();
    m_color = color;
    m_opacity = opacity;
    m_quantity = quantity;
    m_label = label;
  }

  public Color getColor( )
  {
    return m_color;
  }

  public void setColor( Color color )
  {
    m_color = color;
  }

  public String getLabel( )
  {
    return m_label;
  }

  public void setLabel( String label )
  {
    m_label = label;
  }

  public double getOpacity( )
  {
    return m_opacity;
  }

  public void setOpacity( double opacity )
  {
    m_opacity = opacity;
  }

  public double getQuantity( )
  {
    return m_quantity;
  }

  public void setQuantity( double quantity )
  {
    m_quantity = quantity;
  }

  public net.opengis.sld.ColorMapEntry getColorMapEntry( )
  {
    net.opengis.sld.ColorMapEntry colorMapEntry = OF.createColorMapEntry();
    colorMapEntry.setColor( StyleFactory.getColorAsHex( getColor() ) );
    //colorMapEntry.setLabel( getLabel() );
    //colorMapEntry.setOpacity( getOpacity() );
    //colorMapEntry.setQuantity( getQuantity() );

    return colorMapEntry;
  }

  public String exportAsXML( )
  {
    StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<ColorMapEntry" );
    sb.append( " color=\"" ).append( StyleFactory.getColorAsHex( getColor() ) ).append( "\"" );
    sb.append( " opacity=\"0.8\"" );
    sb.append( " quantity=\"" ).append( getQuantity() ).append( "\"" );
    sb.append( "/>" );
    return sb.toString();
  }

  @Override
  public ColorMapEntry clone( )
  {
    return new ColorMapEntry_Impl( m_color, m_opacity, m_quantity, m_label );
  }

}