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
package org.kalypso.commons.parser.impl;

import org.kalypso.commons.parser.AbstractParser;
import org.kalypso.commons.parser.ParserException;

/**
 * Ein Parser für Integer, Long, und Short Objekte.
 * 
 * @author schlienger
 */
public class BooleanParser extends AbstractParser
{
  private final String m_format;

  /**
   * Default Constructor
   */
  public BooleanParser()
  {
    this( "" );
  }

  /**
   * @param format
   *          hat keine Bedeutung für Integers
   */
  public BooleanParser( final String format )
  {
    m_format = format;
  }

  /**
   * @see org.kalypso.commons.parser.IParser#getObjectClass()
   */
  public Class getObjectClass()
  {
    return Boolean.class;
  }

  /**
   * @see org.kalypso.commons.parser.IParser#getFormat()
   */
  public String getFormat()
  {
    return m_format;
  }

  /**
   * @throws ParserException
   * @see org.kalypso.commons.parser.IParser#parse(java.lang.String)
   */
  public Object parse( final String text ) throws ParserException
  {
    return Boolean.valueOf( text );
  }

  /**
   * @see org.kalypso.commons.parser.AbstractParser#toStringInternal(java.lang.Object)
   */
  public String toStringInternal( Object obj )
  {
    return ((Boolean)obj).toString();
  }

  /**
   * @see org.kalypso.commons.parser.IParser#compare(java.lang.Object, java.lang.Object)
   */
  public int compare( Object value1, Object value2 )
  {
    if( value1.equals(value2) )
        return 0;
    
    if( value1.equals(Boolean.FALSE ) )
        return 1;
    
    return -1;
  }
}