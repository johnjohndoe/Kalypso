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
 * Ein Parser f�r Double und Float Objekte.
 * 
 * @author schlienger
 */
public class DoubleParser extends AbstractParser
{

  /**
   * Default constructor: calls DoubleParser( "" )
   */
  public DoubleParser()
  {
    this( "" );
  }

  /**
   * @param format
   *          siehe Spezifikation in DecimalFormat
   * @see java.text.DecimalFormat
   */
  public DoubleParser( String format )
  {

    format.getClass();
    //
  }

  /**
   * @see org.kalypso.commons.parser.IParser#getObjectClass()
   */
  public Class getObjectClass()
  {
    return Double.class;
  }

  /**
   * @see org.kalypso.commons.parser.IParser#getFormat()
   */
  public String getFormat()
  {
    return "";
  }

  /**
   * @see org.kalypso.commons.parser.IParser#parse(java.lang.String)
   */
  public Object parse( String text ) throws ParserException
  {
    try
    {
      return Double.valueOf( text );
    }
    catch( NumberFormatException e )
    {
      throw new ParserException( e );
    }
  }

  /**
   * @see org.kalypso.commons.parser.AbstractParser#toStringInternal(java.lang.Object)
   */
  @Override
  public String toStringInternal( Object obj )
  {
    return obj.toString();
  }

  /**
   * @see org.kalypso.commons.parser.IParser#compare(java.lang.Object, java.lang.Object)
   */
  public int compare( Object value1, Object value2 )
  {
    double n1 = ( (Number)value1 ).doubleValue();
    double n2 = ( (Number)value2 ).doubleValue();

    return Double.compare( n1, n2 );
  }
}