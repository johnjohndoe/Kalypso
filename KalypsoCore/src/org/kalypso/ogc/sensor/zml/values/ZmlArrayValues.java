/*--------------- Kalypso-Header --------------------------------------------------------------------

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
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.zml.values;

import java.util.List;
import java.util.StringTokenizer;
import java.util.Vector;

import org.kalypso.util.parser.IParser;
import org.kalypso.util.parser.ParserException;
import org.kalypso.zml.AxisType.ValueArrayType;

/**
 * @author schlienger
 */
public class ZmlArrayValues implements IZmlValues
{
  private final List m_values;

  public ZmlArrayValues( final ValueArrayType va, final IParser parser ) throws ParserException
  {
    final StringTokenizer stok = new StringTokenizer( va.getValue(), va.getSeparator() );

    m_values = new Vector( stok.countTokens() );

    while( stok.hasMoreElements() )
    {
      final String token = stok.nextToken();
      final Object obj = parser.parse( token );
      m_values.add( obj );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#getElement(int)
   */
  public Object getElement( int index )
  {
    return m_values.get( index );
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#setElement(int,
   *      java.lang.Object)
   */
  public void setElement( int index, Object element )
  {
    m_values.set( index, element );
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#getCount()
   */
  public int getCount()
  {
    return m_values.size();
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#indexOf(java.lang.Object)
   */
  public int indexOf( final Object obj )
  {
    return m_values.indexOf( obj );
  }
}