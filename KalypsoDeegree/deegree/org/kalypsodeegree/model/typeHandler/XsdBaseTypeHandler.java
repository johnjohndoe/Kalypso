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

import java.net.URL;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.types.ISimpleMarshallingTypeHandler;
import org.kalypso.gmlschema.types.UnmarshallResultEater;
import org.xml.sax.XMLReader;

/**
 * Base type-handler for simple types.
 * 
 * @author Andreas von Dömming
 */
public abstract class XsdBaseTypeHandler<T> implements ISimpleMarshallingTypeHandler<T>, Comparator<T>
{
  private final QName m_typeQName;

  private final Class<T> m_valueClass;

  public XsdBaseTypeHandler( final String xsdTypeName, final Class<T> valueClass )
  {
    m_valueClass = valueClass;
    m_typeQName = new QName( NS.XSD_SCHEMA, xsdTypeName );
  }

  public XsdBaseTypeHandler( final QName qname, final Class<T> valueClass )
  {
    m_valueClass = valueClass;
    m_typeQName = qname;
  }

  /**
   * @see org.kalypso.gmlschema.types.IMarshallingTypeHandler#marshal(java.lang.Object, org.xml.sax.XMLReader,
   *      java.net.URL, java.lang.String)
   */
  public final void marshal( final Object value, final XMLReader reader, final URL context, final String gmlVersion )
  {
    throw new UnsupportedOperationException();
  }

  public abstract String convertToXMLString( final T value );

  public abstract T convertToJavaValue( final String xmlString );

  /**
   * @see org.kalypso.gmlschema.types.IMarshallingTypeHandler#unmarshal(org.xml.sax.XMLReader,
   *      org.kalypso.contribs.java.net.IUrlResolver, org.kalypso.gmlschema.types.MarshalResultEater)
   */
  public void unmarshal( final XMLReader xmlReader, final URL context, final UnmarshallResultEater marshalResultEater, final String gmlVersion )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.gmlschema.types.IMarshallingTypeHandler#getShortname()
   */
  public String getShortname( )
  {
    return m_typeQName.getLocalPart();
  }

  /**
   * @see org.kalypso.gmlschema.types.IMarshallingTypeHandler#cloneObject(java.lang.Object)
   */
  public Object cloneObject( final Object objectToClone, final String gmlVersion ) throws CloneNotSupportedException
  {
    if( objectToClone == null )
      return null;

    try
    {
      if( objectToClone instanceof List )
      {
        final List<T> list = (List<T>) objectToClone;
        final List<T> clonedList = new ArrayList<T>( list.size() );
        for( final Object listItem : list )
        {
          final T clonedObject = cloneValueObject( (T) listItem );
          clonedList.add( clonedObject );
        }
        return clonedList;
      }
      // no list
      return cloneValueObject( (T) objectToClone );
    }
    catch( final Exception p )
    {
      throw new CloneNotSupportedException( p.getMessage() );
    }
  }

  private T cloneValueObject( final T objectToClone )
  {
    final String xmlString = convertToXMLString( objectToClone );
    return convertToJavaValue( xmlString );
  }

  /**
   * @see org.kalypso.gmlschema.types.IMarshallingTypeHandler#parseType(java.lang.String)
   */
  public Object parseType( final String xmlString ) throws ParseException
  {
    if( xmlString == null || xmlString.isEmpty() )
      return null;

    try
    {
      return convertToJavaValue( xmlString );
    }
    catch( final Exception e )
    {
      throw new ParseException( xmlString, 0 );
    }
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#getValueClass()
   */
  public Class<T> getValueClass( )
  {
    return m_valueClass;
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#getTypeName()
   */
  public QName getTypeName( )
  {
    return m_typeQName;
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#isGeometry()
   */
  public boolean isGeometry( )
  {
    return false;
  }

}
