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
package org.kalypso.util.parser.impl;

import org.kalypso.util.parser.AbstractParser;

/**
 * StringParser
 * 
 * @author schlienger
 */
public class StringParser extends AbstractParser
{
  /**
   * @see org.kalypso.util.parser.AbstractParser#toStringInternal(java.lang.Object)
   */
  public String toStringInternal( Object obj )
  {
    return obj.toString();
  }

  /**
   * @see org.kalypso.util.parser.IParser#getObjectClass()
   */
  public Class getObjectClass()
  {
    return String.class;
  }

  /**
   * @see org.kalypso.util.parser.IParser#getFormat()
   */
  public String getFormat()
  {
    return "";
  }

  /**
   * @see org.kalypso.util.parser.IParser#parse(java.lang.String)
   */
  public Object parse( String text )
  {
    return text;
  }

  /**
   * @see org.kalypso.util.parser.IParser#compare(java.lang.String, java.lang.String)
   */
  public int compare( String value1, String value2 )
  {
    if( value1.compareTo( value2 ) == 0 )
      return 0;

    if( value1.length() < value2.length() )
      return -1;

    return 1;
  }

  /**
   * @see org.kalypso.util.parser.IParser#compare(java.lang.Object, java.lang.Object)
   */
  public int compare( Object value1, Object value2 )
  {
    return compare( value1.toString(), value2.toString() );
  }
}
