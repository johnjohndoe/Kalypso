/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import javax.naming.OperationNotSupportedException;
import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler2;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypso.gmlschema.types.UnmarshallResultEater;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.io.sax.TriangulatedSurfaceContentHandler;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.XMLReader;
import org.xml.sax.ext.LexicalHandler;

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
   *      org.xml.sax.ContentHandler, org.xml.sax.ext.LexicalHandler, java.net.URL, java.lang.String)
   */
  public void marshal( final QName propQName, final Object value, final ContentHandler contentHandler, final LexicalHandler lexicalHandler, final URL context, final String gmlVersion ) throws TypeRegistryException
  {
    throw new TypeRegistryException( new OperationNotSupportedException() );
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
