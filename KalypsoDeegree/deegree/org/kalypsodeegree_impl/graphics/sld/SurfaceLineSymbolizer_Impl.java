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

import org.kalypsodeegree.graphics.sld.Geometry;
import org.kalypsodeegree.graphics.sld.SurfaceLineSymbolizer;
import org.kalypsodeegree.xml.Marshallable;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * Used to render the isolines of triangulated surface geometry.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author Thomas Jung
 */
public class SurfaceLineSymbolizer_Impl extends Symbolizer_Impl implements SurfaceLineSymbolizer, Marshallable
{
  private LineColorMap m_colorMap;

  /**
   * Creates a new PolygonSymbolizer_Impl object.
   */
  public SurfaceLineSymbolizer_Impl( )
  {
    this( new LineColorMap_Impl(), null, 0, 99E9, UOM.pixel );
  }

  /**
   * constructor initializing the class with the <PolygonSymbolizer>
   */
  public SurfaceLineSymbolizer_Impl( LineColorMap colorMap, final Geometry geometry, final double min, final double max, final UOM uom )
  {
    super( geometry, uom );
    setColorMap( colorMap );
    setMinScaleDenominator( min );
    setMaxScaleDenominator( max );
  }

  public void setColorMap( final LineColorMap colorMap )
  {
    m_colorMap = colorMap;
  }

  public LineColorMap getColorMap( )
  {
    return m_colorMap;
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
    sb.append( "<SurfaceLineSymbolizer xmlns:sldExt=\""+ SLDFactory.SLDNS_EXT + "\">\n" );

    if( getGeometry() != null )
    {
      sb.append( getGeometry() ).append( "\n" );
    }

    if( getColorMap() != null )
    {
      sb.append( getColorMap() ).append( "\n" );
    }

    sb.append( "</SurfaceLineSymbolizer>\n" );

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
    sb.append( "<SurfaceLineSymbolizer xmlns:sldExt=\""+ SLDFactory.SLDNS_EXT + "\">\n" );
    final Geometry geometry = getGeometry();
    if( geometry != null )
    {
      sb.append( ((Marshallable) geometry).exportAsXML() );
    }
    if( m_colorMap != null )
    {
      sb.append( ((Marshallable) m_colorMap).exportAsXML() );
    }
    sb.append( "</SurfaceLineSymbolizer>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }

}