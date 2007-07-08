/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import java.util.LinkedList;
import java.util.List;

import org.kalypsodeegree.graphics.sld.LineColorMapEntry;
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
    sb.append( "<SurfacePolygonSymbolizer>" );
    sb.append( "<PolygonColorMap>" );

    if( m_colorMap != null )
    {
      for( int i = 0; i < m_colorMap.size(); i++ )
      {
        final PolygonColorMapEntry polygonColorMapEntry = m_colorMap.get( i );
        sb.append( polygonColorMapEntry.exportAsXML() );
      }
    }

    sb.append( "</PolygonColorMap>" );
    sb.append( "</SurfacePolygonSymbolizer>" );

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

}
