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
package org.kalypsodeegree_impl.graphics.sld;

import org.kalypsodeegree.graphics.sld.Geometry;
import org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer;
import org.kalypsodeegree.xml.Marshallable;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * @author Thomas Jung
 */
public class SurfacePolygonSymbolizer_Impl extends Symbolizer_Impl implements SurfacePolygonSymbolizer, Marshallable
{
  private PolygonColorMap m_colorMap;

  /**
   * Creates a new PolygonSymbolizer_Impl object.
   */
  public SurfacePolygonSymbolizer_Impl( )
  {
    this( new PolygonColorMap_Impl(), null, 0, 99E9, UOM.pixel );
  }

  /**
   * constructor initializing the class with the <PolygonSymbolizer>
   */
  public SurfacePolygonSymbolizer_Impl( PolygonColorMap colorMap, final Geometry geometry, final double min, final double max, final UOM uom )
  {
    super( geometry, uom );
    setColorMap( colorMap );
    setMinScaleDenominator( min );
    setMaxScaleDenominator( max );
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer#getColorMap()
   */
  public PolygonColorMap getColorMap( )
  {
    return m_colorMap;
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.SurfacePolygonSymbolizer#setColorMap(org.kalypsodeegree_impl.graphics.sld.PolygonColorMap)
   */
  public void setColorMap( PolygonColorMap colorMap )
  {
    m_colorMap = colorMap;
  }

  /**
   * Produces a textual representation of this object.
   * 
   * @return the textual representation
   */
  @Override
  public String toString( )
  {
    final StringBuffer sb = new StringBuffer();
    sb.append( "scale constraint:  >=" + getMinScaleDenominator() + " AND <" + getMaxScaleDenominator() + "\n" );
    sb.append( "<SurfacePolygonSymbolizer xmlns:sldExt=\"" + SLDFactory.SLDNS_EXT + "\"" );

    final UOM uom = getUom();

    if( uom != null )
    {
      sb.append( " uom=\"" + uom.name() + "\">" );
    }
    else
      sb.append( ">\n" );

    if( getGeometry() != null )
    {
      sb.append( getGeometry() ).append( "\n" );
    }

    if( getColorMap() != null )
    {
      sb.append( getColorMap() ).append( "\n" );
    }

    sb.append( "</SurfacePolygonSymbolizer>\n" );

    return sb.toString();
  }

  /**
   * exports the content of the PolygonSymbolizer as XML formated String
   * 
   * @return xml representation of the PolygonSymbolizer
   */
  public String exportAsXML( )
  {
    Debug.debugMethodBegin();

    final StringBuffer sb = new StringBuffer( 1000 );

    sb.append( "<SurfacePolygonSymbolizer xmlns:sldExt=\"" + SLDFactory.SLDNS_EXT + "\"" );

    final UOM uom = getUom();

    if( uom != null )
    {
      sb.append( " uom=\"" + uom.name() + "\">" );
    }
    else
      sb.append( ">\n" );

    final Geometry geometry = getGeometry();
    if( geometry != null )
    {
      sb.append( ((Marshallable) geometry).exportAsXML() );
    }
    if( m_colorMap != null )
    {
      sb.append( ((Marshallable) m_colorMap).exportAsXML() );
    }
    sb.append( "</SurfacePolygonSymbolizer>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }

}
