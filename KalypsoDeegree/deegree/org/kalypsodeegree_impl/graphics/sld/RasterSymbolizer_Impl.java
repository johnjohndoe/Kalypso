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
import java.util.Iterator;
import java.util.TreeMap;

import org.kalypsodeegree.graphics.sld.ColorMapEntry;
import org.kalypsodeegree.graphics.sld.Interval;
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
  private TreeMap m_colorMap = null;

  private final int mode_intervalColorMapping = 0;

  private int m_mode = mode_intervalColorMapping;

  public RasterSymbolizer_Impl( final TreeMap colorMap )
  {
    setColorMap( colorMap );
  }

  public TreeMap getColorMap( )
  {
    return m_colorMap;
  }

  public void setColorMap( final TreeMap colorMap )
  {
    m_colorMap = colorMap;
  }

  public int getMode( )
  {
    return m_mode;
  }

  public void setMode( final int mode )
  {
    this.m_mode = mode;
  }

  public TreeMap getIntervalMap( )
  {
    final TreeMap intervalMap = new TreeMap();
    final Object[] colorMapKeys = m_colorMap.keySet().toArray();
    int startIndex = 0;
    final double nullValue = -9999;
    if( ((Double) colorMapKeys[0]).doubleValue() == nullValue )
    {
      startIndex = 1;
    }
    for( int i = startIndex; i < colorMapKeys.length - 1; i++ )
    {
      final ColorMapEntry colorMapEntry_i = (ColorMapEntry) m_colorMap.get( colorMapKeys[i] );
      final ColorMapEntry colorMapEntry_i1 = (ColorMapEntry) m_colorMap.get( colorMapKeys[i + 1] );
      final Interval interval = new Interval_Impl( colorMapEntry_i.getQuantity(), colorMapEntry_i1.getQuantity() );
      final Color color = colorMapEntry_i.getColor();
      // TODO Opacity allways 0, check this
      final Color colorWithOpacity = new Color( color.getRed(), color.getGreen(), color.getBlue(), (int) Math.round( colorMapEntry_i.getOpacity() * 255 ) );
      intervalMap.put( interval, colorWithOpacity );
    }
    return intervalMap;
  }

  public String exportAsXML( )
  {
    final StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<RasterSymbolizer>" );
    sb.append( "<ColorMap>" );

    if( m_colorMap != null )
    {
      final Iterator it = m_colorMap.keySet().iterator();
      while( it.hasNext() )
      {
        final ColorMapEntry colorMapEntry = (ColorMapEntry) m_colorMap.get( it.next() );
        sb.append( colorMapEntry.exportAsXML() );
      }
    }

    sb.append( "</ColorMap>" );
    sb.append( "</RasterSymbolizer>" );

    return sb.toString();
  }

}