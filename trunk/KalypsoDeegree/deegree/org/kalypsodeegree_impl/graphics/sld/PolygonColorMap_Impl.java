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

import java.util.LinkedList;
import java.util.List;

import org.kalypsodeegree.graphics.sld.PolygonColorMapEntry;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Thomas Jung
 */
public class PolygonColorMap_Impl implements PolygonColorMap
{

  private final List<PolygonColorMapEntry> m_colorMap = new LinkedList<PolygonColorMapEntry>();

  /**
   * @see org.kalypsodeegree_impl.graphics.sld.PolygonColorMap#getColorMap()
   */
  public PolygonColorMapEntry[] getColorMap( )
  {
    return m_colorMap.toArray( new PolygonColorMapEntry[m_colorMap.size()] );
  }

  public void setColorMap( final List<PolygonColorMapEntry> colorMap )
  {
    m_colorMap.addAll( colorMap );
  }

  public void addColorMapClass( final PolygonColorMapEntry colorMapEntry )
  {
    m_colorMap.add( colorMapEntry );
  }

  public String exportAsXML( )
  {
    final StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<sldExt:PolygonColorMap>" );

    if( m_colorMap != null )
    {
      for( int i = 0; i < m_colorMap.size(); i++ )
      {
        final PolygonColorMapEntry polygonColorMapEntry = m_colorMap.get( i );
        sb.append( polygonColorMapEntry.exportAsXML() );
      }
    }

    sb.append( "</sldExt:PolygonColorMap>" );

    return sb.toString();

  }

  /**
   * @see org.kalypsodeegree_impl.graphics.sld.PolygonColorMap#findEntry(java.lang.String,
   *      org.kalypsodeegree.model.feature.Feature)
   */
  public PolygonColorMapEntry findEntry( String label, Feature feature )
  {
    for( final PolygonColorMapEntry entry : m_colorMap )
    {
      if( entry.getLabel( feature ).equals( label ) )
        return entry;
    }

    return null;
  }

  /**
   * @see org.kalypsodeegree_impl.graphics.sld.PolygonColorMap#replaceColorMap(java.util.List)
   */
  public void replaceColorMap( List<PolygonColorMapEntry> colorMapList )
  {
    m_colorMap.clear();
    m_colorMap.addAll( colorMapList );
  }

}
