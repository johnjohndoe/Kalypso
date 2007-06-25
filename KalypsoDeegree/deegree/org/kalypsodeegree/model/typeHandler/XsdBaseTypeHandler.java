/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypsodeegree.model.typeHandler;

import java.net.URL;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.types.ISimpleMarshallingTypeHandler;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypso.gmlschema.types.UnmarshallResultEater;
import org.kalypso.gmlschema.types.XsdBaseContentHandler;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.ext.LexicalHandler;

/**
 * @author doemming
 */
public abstract class XsdBaseTypeHandler<T> implements ISimpleMarshallingTypeHandler<T>, Comparator<T>
{
  private final XsdBaseContentHandler m_contentHandler = new XsdBaseContentHandler( this, null );

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
   * @see org.kalypso.gmlschema.types.IMarshallingTypeHandler#marshal(javax.xml.namespace.QName, java.lang.Object,
   *      org.xml.sax.ContentHandler, org.xml.sax.ext.LexicalHandler, java.net.URL)
   */
  public final void marshal( final QName propQName, final Object value, final XMLReader reader, final URL context, final String gmlVersion ) throws SAXException
  {
    final ContentHandler contentHandler = reader.getContentHandler();
    final LexicalHandler lexicalHandler = (LexicalHandler) reader.getProperty( "http://xml.org/sax/properties/lexical-handler" );

    // TODO: this is NOT the right place to write the element! This should be done in the next higher level, because
    // it is always the same for every marshalling type handler.
    final String namespaceURI = propQName.getNamespaceURI();
    final String localPart = propQName.getLocalPart();
    final String qNameString = propQName.getPrefix() + ":" + localPart;
    contentHandler.startElement( namespaceURI, localPart, qNameString, null );

    // FIXME: this is the right place to write CDATA stuff, but of course now it is a wild hack
    // to look for a specific value. This must of course be decided in a more generel way.
    // Maybe we register extensions for specific qnames?
    // TODO: also, it should be only done for String, i.e. in the XxsdBaseTypeHandlerString
    if( propQName.equals( new QName( NS.OM, "result" ) ) )
      lexicalHandler.startCDATA();

    final String valueAsXMLString = convertToXMLString( (T) value );
    final char[] cs = valueAsXMLString.toCharArray();
    contentHandler.characters( cs, 0, cs.length );

    if( propQName.equals( new QName( NS.OM, "result" ) ) )
      lexicalHandler.endCDATA();

    contentHandler.endElement( namespaceURI, localPart, qNameString );
  }

  public abstract String convertToXMLString( final T value );

  public abstract T convertToJavaValue( final String xmlString );

  /**
   * @see org.kalypso.gmlschema.types.IMarshallingTypeHandler#unmarshal(org.xml.sax.XMLReader,
   *      org.kalypso.contribs.java.net.IUrlResolver, org.kalypso.gmlschema.types.MarshalResultEater)
   */
  public void unmarshal( final XMLReader xmlReader, final URL context, final UnmarshallResultEater marshalResultEater, final String gmlVersion ) throws TypeRegistryException
  {
    try
    {
      // REMARK: We had a small performance and memory problem here, because each time the method
      // was called a content handler (and severel other classes) where instantiated.
      // But this method is called quite often!
      // We now reuse the same content handler. This is safe, because a simple type never contains any other types.
      m_contentHandler.setMarshalResultEater( marshalResultEater, true );

      xmlReader.setContentHandler( m_contentHandler );
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
      throw new TypeRegistryException( e );
    }
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
    {
      return null;
    }
    try
    {// TODO: only do this in the case of the list-handler?
      if( objectToClone instanceof List )
      {
        final List list = (List) objectToClone;
        final List clonedList = new ArrayList( list.size() );
        for( final Object listItem : list )
        {
          final String stringOfCloneItem = convertToXMLString( (T) listItem );
          clonedList.add( parseType( stringOfCloneItem ) );
        }
        return clonedList;
      }
      // no list
      final String stringOfClone = convertToXMLString( (T) objectToClone );
      return parseType( stringOfClone );
    }
    catch( final ParseException p )
    {
      throw new CloneNotSupportedException( p.getMessage() );
    }
  }

  /**
   * @see org.kalypso.gmlschema.types.IMarshallingTypeHandler#parseType(java.lang.String)
   */
  public Object parseType( final String xmlString ) throws ParseException
  {
    if( (xmlString == null) || (xmlString.length() < 1) )
    {
      return null;
    }
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
