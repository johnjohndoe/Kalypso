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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import org.kalypso.util.parser.AbstractParser;
import org.kalypso.util.parser.ParserException;

/**
 * Ein Parser für Date Objekte.
 * 
 * @author schlienger
 */
public class DateParser extends AbstractParser
{
  private final SimpleDateFormat m_df;

  private final String m_format;

  /**
   * Default constructor.
   */
  public DateParser()
  {
    this( "" );
  }

  /**
   * @param format
   *          siehe Spezifikation in SimpleDateFormat
   * @see SimpleDateFormat
   */
  public DateParser( String format )
  {
    m_format = format;

    if( format.length() == 0 )
      m_df = new SimpleDateFormat();
    else
      m_df = new SimpleDateFormat( format );
  }

  /**
   * @see org.kalypso.util.parser.IParser#getObjectClass()
   */
  public Class getObjectClass()
  {
    return Date.class;
  }

  /**
   * @see org.kalypso.util.parser.IParser#getFormat()
   */
  public String getFormat()
  {
    return m_format;
  }

  /**
   * @throws ParserException
   * @see org.kalypso.util.parser.IParser#parse(java.lang.String)
   */
  public Object parse( String text ) throws ParserException
  {
    try
    {
      return m_df.parse( text );
    }
    catch( ParseException e )
    {
      throw new ParserException( e );
    }
  }

  /**
   * @see org.kalypso.util.parser.AbstractParser#toStringInternal(java.lang.Object)
   */
  public String toStringInternal( Object obj )
  {
    return m_df.format( obj );
  }

  /**
   * @see org.kalypso.util.parser.IParser#compare(java.lang.Object,
   *      java.lang.Object)
   */
  public int compare( Object value1, Object value2 )
  {
    Date d1 = (Date) value1;
    Date d2 = (Date) value2;

    return d1.compareTo( d2 );
  }
  
  public void setTimezone( final TimeZone tz )
  {
    if( tz != null )
      m_df.setTimeZone( tz );
    else
      m_df.setTimeZone( TimeZone.getDefault() );
  }
}