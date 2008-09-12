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
package org.kalypso.commons.xml;

import java.util.Date;
import java.util.TimeZone;

import javax.xml.namespace.QName;

import org.kalypso.commons.parser.impl.DateParser;
import org.kalypso.commons.parser.impl.DoubleParser;

/**
 * Utility class that contains the default java formaters for the types found in xml.
 * 
 * @author schlienger
 */
public final class XmlTypes
{
  private XmlTypes( )
  {
    // do not instantiate
  }

  // TODO: move these into an interface, so they can be better included (via implements) for heavy use
  public final static QName XS_BOOLEAN = new QName( NS.XSD_SCHEMA, "boolean" );//$NON-NLS-1$

  public final static QName XS_BYTE = new QName( NS.XSD_SCHEMA, "byte" );//$NON-NLS-1$

  public final static QName XS_DATE = new QName( NS.XSD_SCHEMA, "date" );//$NON-NLS-1$

  public final static QName XS_DATETIME = new QName( NS.XSD_SCHEMA, "dateTime" );//$NON-NLS-1$

  public final static QName XS_DECIMAL = new QName( NS.XSD_SCHEMA, "decimal" );//$NON-NLS-1$

  public final static QName XS_DOUBLE = new QName( NS.XSD_SCHEMA, "double" );//$NON-NLS-1$

  public final static QName XS_DURATION = new QName( NS.XSD_SCHEMA, "duration" );//$NON-NLS-1$

  public final static QName XS_FLOAT = new QName( NS.XSD_SCHEMA, "float" );//$NON-NLS-1$

  public final static QName XS_INT = new QName( NS.XSD_SCHEMA, "int" );//$NON-NLS-1$

  public final static QName XS_INTEGER = new QName( NS.XSD_SCHEMA, "integer" );//$NON-NLS-1$

  public final static QName XS_LONG = new QName( NS.XSD_SCHEMA, "long" );//$NON-NLS-1$

  public final static QName XS_SHORT = new QName( NS.XSD_SCHEMA, "short" );//$NON-NLS-1$

  public final static QName XS_STRING = new QName( NS.XSD_SCHEMA, "string" );//$NON-NLS-1$

  public final static QName XS_TIME = new QName( NS.XSD_SCHEMA, "time" );//$NON-NLS-1$

  public final static String XML_DATETIME_FORMAT = "yyyy-MM-dd'T'HH:mm:ss";//$NON-NLS-1$

  /**
   * Parser for the type <code>double</code>. It uses the default behaviour of the java.lang.Double class.
   */
  public final static DoubleParser PDOUBLE = new DoubleParser();

  /**
   * Returns true if the given QName represents an xml-type which is a number
   */
  public static boolean isNumber( final QName valueTypeName )
  {
    return valueTypeName.equals( XmlTypes.XS_BYTE ) || valueTypeName.equals( XmlTypes.XS_DECIMAL ) || valueTypeName.equals( XmlTypes.XS_DOUBLE ) || valueTypeName.equals( XmlTypes.XS_FLOAT )
        || valueTypeName.equals( XmlTypes.XS_INT ) || valueTypeName.equals( XmlTypes.XS_INTEGER ) || valueTypeName.equals( XmlTypes.XS_LONG ) || valueTypeName.equals( XmlTypes.XS_SHORT );
  }

  /**
   * Returns true if the given QName represents an xml-type which is a date
   */
  public static boolean isDate( final QName valueTypeName )
  {
    return valueTypeName.equals( XmlTypes.XS_DATE ) || valueTypeName.equals( XmlTypes.XS_DATETIME ) || valueTypeName.equals( XmlTypes.XS_DURATION ) || valueTypeName.equals( XmlTypes.XS_TIME );
  }

  /**
   * Parser for the type <code>date</code>. It uses following format string:
   * 
   * <pre>
   *      yyyy-MM-dd'T'HH:mm:ss
   * </pre>
   */
  public static DateParser getDateParser( final TimeZone timezone )
  {
    final DateParser parser = new DateParser( XML_DATETIME_FORMAT );
    parser.setTimezone( timezone );

    return parser;
  }

  /**
   * @return the corresponding java class to the given QName representing an XML type.
   */
  public static Class< ? > toJavaClass( final QName typeName )
  {
    if( XS_BOOLEAN.equals( typeName ) )
      return Boolean.class;

    if( XS_BYTE.equals( typeName ) )
      return Byte.class;

    if( isDate( typeName ) )
      return Date.class;

    if( XS_DECIMAL.equals( typeName ) || XS_DOUBLE.equals( typeName ) )
      return Double.class;

    if( XS_FLOAT.equals( typeName ) )
      return Float.class;

    if( XS_INT.equals( typeName ) || XS_INTEGER.equals( typeName ) )
      return Integer.class;

    if( XS_LONG.equals( typeName ) )
      return Long.class;

    if( XS_SHORT.equals( typeName ) )
      return Short.class;

    if( XS_STRING.equals( typeName ) )
      return String.class;

    return Object.class;
  }
}