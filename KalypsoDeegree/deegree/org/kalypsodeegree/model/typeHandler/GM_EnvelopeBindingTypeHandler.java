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

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.UnmarshallerHandler;
import javax.xml.namespace.QName;

import org.kalypso.commons.bind.JaxbUtilities;
import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.types.BindingUnmarshalingContentHandler;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.JAXBContextProvider;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypso.gmlschema.types.UnmarshalResultProvider;
import org.kalypso.gmlschema.types.UnmarshallResultEater;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.geometry.AdapterBindingToValue;
import org.kalypsodeegree_impl.model.geometry.AdapterGmlIO;
import org.kalypsodeegree_impl.model.geometry.AdapterValueToGMLBinding;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.AttributesImpl;

/**
 * a generic typehandler for GM_Object geometries based on a bindingtypehandler<br>
 * is wraps binding geometries to GM_Object geometries
 * 
 * @author doemming
 */
public class GM_EnvelopeBindingTypeHandler implements IMarshallingTypeHandler
{
  private static final QName QNAME_TAG_GML2 = new QName( NS.GML2, "Box" );

  private static final QName QNAME_TAG_GML3 = new QName( NS.GML3, "Envelope" );

  private final JAXBContextProvider m_jaxbContextProvider;

  private final QName m_xmlTypeQName;

  private final Class< ? > m_valueClass;

  private final boolean m_isGeometry;

  public GM_EnvelopeBindingTypeHandler( final JAXBContextProvider jaxbContextProvider, final QName xmlTypeQName, final Class< ? > gm_objectClass, final boolean isGeometry )
  {
    m_jaxbContextProvider = jaxbContextProvider;
    m_xmlTypeQName = xmlTypeQName;

    m_valueClass = gm_objectClass;
    m_isGeometry = isGeometry;
  }

  /**
   * @see org.kalypso.gmlschema.types.GenericBindingTypeHandler#unmarshal(org.xml.sax.XMLReader,
   *      org.kalypso.contribs.java.net.IUrlResolver, org.kalypso.gmlschema.types.MarshalResultEater)
   */
  public void unmarshal( final XMLReader xmlReader, final URL context, final UnmarshallResultEater marshalResultEater, final String gmlVersion ) throws TypeRegistryException
  {
    final UnmarshallResultEater eater = new UnmarshallResultEater()
    {
      public void unmarshallSuccesful( final Object bindingGeometry ) throws SAXParseException
      {
        try
        {
          final Class< ? > geometryClass = getValueClass();
          final AdapterBindingToValue bindingToGM_ObjectAdapter = AdapterGmlIO.getGMLBindingToGM_ObjectAdapter( gmlVersion );
          final Object geometryValue = bindingToGM_ObjectAdapter.wrapFromBinding( bindingGeometry, geometryClass );
          marshalResultEater.unmarshallSuccesful( geometryValue );
        }
        catch( final Exception e )
        {
          throw new SAXParseException( e.getLocalizedMessage(), null, e );
        }
      }
    };

    try
    {
      final JAXBContext jaxbContext = m_jaxbContextProvider.getJaxBContextForGMLVersion( gmlVersion );
      final Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
      final UnmarshallerHandler unmarshallerHandler = unmarshaller.getUnmarshallerHandler();
      final UnmarshalResultProvider provider = new UnmarshalResultProvider()
      {
        public Object getResult( ) throws GMLSchemaException
        {
          try
          {
            unmarshallerHandler.endDocument();
            return unmarshallerHandler.getResult();
          }
          catch( final Exception e )
          {
            throw new GMLSchemaException( e );
          }
        }
      };
      final BindingUnmarshalingContentHandler tmpContentHandler = new BindingUnmarshalingContentHandler( unmarshallerHandler, provider, eater, false, gmlVersion );
      tmpContentHandler.startDocument();
      xmlReader.setContentHandler( tmpContentHandler );
    }
    catch( final Exception e )
    {
      throw new TypeRegistryException( e );
    }

  }

  /**
   * @see org.kalypso.gmlschema.types.GenericBindingTypeHandler#marshal(java.lang.Object, org.xml.sax.ContentHandler,
   *      org.xml.sax.ext.LexicalHandler, java.net.URL)
   */
  public void marshal( final QName propQName, final Object geometry, final XMLReader reader, final URL context, final String gmlVersion ) throws SAXException
  {
    try
    {
      final AdapterValueToGMLBinding valueToGMLBindingAdapter = AdapterGmlIO.getGM_ObjectToGMLBindingAdapter( gmlVersion );
      final Object bindingObject = valueToGMLBindingAdapter.wrapToBinding( (GM_Envelope) geometry );
      final QName xmlTagName = getXMLTagNameForGMLVersion( gmlVersion );

      // memory to xml
      final ContentHandler contentHandler = reader.getContentHandler();

      final String namespaceURI = propQName.getNamespaceURI();
      final String localPart = propQName.getLocalPart();
      final String qNameString = propQName.getPrefix() + ":" + localPart;

      contentHandler.startElement( namespaceURI, localPart, qNameString, new AttributesImpl() );

      final JAXBContext jaxbContext = m_jaxbContextProvider.getJaxBContextForGMLVersion( gmlVersion );
      final Marshaller marshaller = JaxbUtilities.createMarshaller( jaxbContext );
      final JAXBElement<Object> jaxElement = JaxbUtilities.createJaxbElement( xmlTagName, bindingObject );
      marshaller.marshal( jaxElement, contentHandler );

      contentHandler.endElement( namespaceURI, localPart, qNameString );
    }
    catch( final JAXBException e )
    {
      throw new SAXException( e );
    }
    catch( final GM_Exception e )
    {
      throw new SAXException( e );
    }
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#getValueClass()
   */
  public Class< ? > getValueClass( )
  {
    return m_valueClass;
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#getTypeName()
   */
  public QName getTypeName( )
  {
    return m_xmlTypeQName;
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#isGeometry()
   */
  public boolean isGeometry( )
  {
    return m_isGeometry;
  }

  /**
   * @see org.kalypso.gmlschema.types.IMarshallingTypeHandler#getShortname()
   */
  public String getShortname( )
  {
    return m_xmlTypeQName.getLocalPart();
  }

  /**
   * @see org.kalypso.gmlschema.types.IMarshallingTypeHandler#cloneObject(java.lang.Object)
   */
  public Object cloneObject( final Object objectToClone, final String gmlVersion )
  {
    if( objectToClone == null )
      return null;

    return ((GM_Envelope) objectToClone).clone();
  }

  /**
   * @see org.kalypso.gmlschema.types.IMarshallingTypeHandler#parseType(java.lang.String)
   */
  public Object parseType( final String text )
  {
    throw new UnsupportedOperationException();
  }

  private QName getXMLTagNameForGMLVersion( final String gmlVersion )
  {
    if( gmlVersion != null && gmlVersion.startsWith( "3" ) )
      return QNAME_TAG_GML3;
    return QNAME_TAG_GML2; // gml2
  }

}
