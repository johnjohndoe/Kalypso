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
package org.kalypsodeegree.model.typeHandler;

import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;


/**
 * @author Dirk Kuch
 */
public class XsdBaseTypeHandlerXMLGregorianCalendar extends XsdBaseTypeHandler<XMLGregorianCalendar>
{
  private final DatatypeFactory m_dataTypeFactory;

  public XsdBaseTypeHandlerXMLGregorianCalendar( final DatatypeFactory dataTypeFactory, final String xsdTypeName )
  {
    super( xsdTypeName, XMLGregorianCalendar.class );
    m_dataTypeFactory = dataTypeFactory;
  }

  /**
   * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
   */
  @Override
  public XMLGregorianCalendar convertToJavaValue( final String xmlString )
  {
    if( xmlString == null || xmlString.isEmpty() )
      return null;

    return m_dataTypeFactory.newXMLGregorianCalendar( xmlString );
  }

  /**
   * @see org.kalypsodeegree.model.XsdBaseTypeHandler#convertToXMLString(java.lang.Object)
   */
  @Override
  public String convertToXMLString( final XMLGregorianCalendar value )
  {
    return value.toXMLFormat();
  }

  /**
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  public int compare( final XMLGregorianCalendar o1, final XMLGregorianCalendar o2 )
  {
    if( (o1 == null) && (o2 == null) )
    {
      return 0; // equals
    }
    else if( o1 == null )
    {
      return -1; // lesser
    }

    return o1.compare( o2 );
  }

}
