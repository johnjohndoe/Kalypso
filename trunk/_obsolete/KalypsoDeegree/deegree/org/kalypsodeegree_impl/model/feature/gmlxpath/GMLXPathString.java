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
package org.kalypsodeegree_impl.model.feature.gmlxpath;

/**
 * representation of Strings in GMLXPath expressions, used temporary by GMLXPath-compiler
 * 
 * @author doemming
 */
public class GMLXPathString
{
  private final String m_condition;

  private final String m_marked;

  protected GMLXPathString( String condition )
  {
    condition = condition.replaceAll( "^\\[", "" );
    condition = condition.replaceAll( "\\]$", "" );

    // care for strings
    boolean insideQuote = false;
    final StringBuffer buffer = new StringBuffer();
    for( int i = 0; i < condition.length(); i++ )
    {
      final char _char = condition.charAt( i );
      if( '\'' == _char && i > 0 && condition.charAt( i - 1 ) != '\\' )
      {
        insideQuote = !insideQuote;
        buffer.append( '\'' );
        continue;
      }
      if( insideQuote )
        buffer.append( '#' );
      else
        buffer.append( _char );
    }
    m_condition = condition;
    m_marked = buffer.toString();
  }

  public String getMarked( )
  {
    return m_marked;
  }

  public String getCond( )
  {
    return m_condition;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return m_condition + "\n" + m_marked;
  }

}