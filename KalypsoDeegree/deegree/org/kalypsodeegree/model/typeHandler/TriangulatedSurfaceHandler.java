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

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler2;
import org.kalypso.gmlschema.types.UnmarshallResultEater;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.io.sax.TriangulatedSurfaceContentHandler;
import org.kalypsodeegree_impl.io.sax.TriangulatedSurfaceMarshaller;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

/**
 * @author Gernot Belger
 */
public class TriangulatedSurfaceHandler implements IMarshallingTypeHandler2
{
  private static final QName QNAME_TYPE = new QName( NS.GML3, "TriangulatedSurface" );

  /**
   * @see org.kalypso.gmlschema.types.IMarshallingTypeHandler#cloneObject(java.lang.Object, java.lang.String)
   */
  public Object cloneObject( final Object objectToClone, final String gmlVersion ) throws CloneNotSupportedException
  {
    throw new CloneNotSupportedException();
  }

  /**
   * @see org.kalypso.gmlschema.types.IMarshallingTypeHandler#getShortname()
   */
  public String getShortname( )
  {
    return QNAME_TYPE.getLocalPart();
  }

  /**
   * @see org.kalypso.gmlschema.types.IMarshallingTypeHandler#marshal(javax.xml.namespace.QName, java.lang.Object,
   *      org.xml.sax.XMLReader, java.net.URL, java.lang.String)
   */
  public void marshal( final QName propQName, final Object value, final XMLReader reader, final URL context, final String gmlVersion ) throws SAXException
  {
    final ContentHandler contentHandler = reader.getContentHandler();

    final String propNamespace = propQName.getNamespaceURI();

    final String propLocalPart = propQName.getLocalPart();
    final String propPrefix = propQName.getPrefix();
    final String propQname = propPrefix + ":" + propLocalPart;

    contentHandler.startElement( propNamespace, propLocalPart, propQname, null );

    final GM_TriangulatedSurface surface = (GM_TriangulatedSurface) value;

    new TriangulatedSurfaceMarshaller( reader, surface ).marshal();

    contentHandler.endElement( propNamespace, propLocalPart, propQname );
  }

  /**
   * @see org.kalypso.gmlschema.types.IMarshallingTypeHandler#parseType(java.lang.String)
   */
  public Object parseType( final String text )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.gmlschema.types.IMarshallingTypeHandler#unmarshal(org.xml.sax.XMLReader, java.net.URL,
   *      org.kalypso.gmlschema.types.UnMarshallResultEater, java.lang.String)
   */
  public void unmarshal( final XMLReader xmlReader, final URL context, final UnmarshallResultEater marshalResultEater, final String gmlVersion )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#getTypeName()
   */
  public QName getTypeName( )
  {
    return QNAME_TYPE;
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#getValueClass()
   */
  public Class< ? > getValueClass( )
  {
    return GM_TriangulatedSurface.class;
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#isGeometry()
   */
  public boolean isGeometry( )
  {
    return true;
  }

  /**
   * @see org.kalypso.gmlschema.types.IMarshallingTypeHandler2#createContentHandler(org.xml.sax.XMLReader,
   *      org.kalypso.gmlschema.types.UnmarshallResultEater, java.lang.String, java.lang.String, java.lang.String,
   *      org.xml.sax.Attributes)
   */
  public ContentHandler createContentHandler( final XMLReader xmlReader, final UnmarshallResultEater resultEater, final String uri, final String localName, final String name, final Attributes atts )
  {
    return new TriangulatedSurfaceContentHandler( resultEater );
  }
}
