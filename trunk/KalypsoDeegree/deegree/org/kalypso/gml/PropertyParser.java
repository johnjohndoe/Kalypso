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
package org.kalypso.gml;

import java.util.List;
import java.util.Stack;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.extension.IMarshallingTypeHandler;
import org.kalypsodeegree_impl.gml.schema.XMLHelper;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

/**
 * @author doemming
 */
public class PropertyParser
{
  private final DocumentBuilder m_builder;

  public PropertyParser( )
  {
    final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setNamespaceAware( true );
    try
    {
      m_builder = factory.newDocumentBuilder();
    }
    catch( ParserConfigurationException e )
    {
      // TODO Auto-generated catch block
      throw new UnsupportedOperationException();
    }
  }

  final Stack<IPropertyType> m_stackPT = new Stack<IPropertyType>();

  public void createProperty( Feature feature, String uri, String localName, Attributes atts )
  {
    final IFeatureType featureType = feature.getFeatureType();
    final QName propQName = new QName( uri, localName );
    final IPropertyType property = featureType.getProperty( propQName );
    m_stackPT.push( property );
    // TODO check if it is a link
  }

  public IPropertyType getCurrentPropertyType( )
  {
    if( m_stackPT.empty() )
      return null;
    return m_stackPT.peek();
  }

  public void setContent( Feature feature, String content )
  {
    return;
    // final IValuePropertyType vpt = (IValuePropertyType) getCurrentPropertyType();
    // final IMarshallingTypeHandler typeHandler = (IMarshallingTypeHandler) vpt.getTypeHandler();

  }

  public void setContent( final Feature parentFE, final IValuePropertyType pt, final XMLReader xmlReader, final String uri, final String localName, final String qName, Attributes atts )
  {

    // This builder is namespace-aware
    final Document doc = m_builder.newDocument();
    final ContentHandler orgCH = xmlReader.getContentHandler();

    final Itest runnable = new Itest()
    {
      public void run( final Node node )
      {
        final IMarshallingTypeHandler th = (IMarshallingTypeHandler) pt.getTypeHandler();
        if( node == null )
          return;
        try
        {
          String test=XMLHelper.toString(node);
//          System.out.println( test );
          final Object value = th.unmarshall( node, null, null );
          if( value == null )
          {
//            System.out.println( "null" );
            return;
          }
          if( pt.isList() )
          {
            final List<Object> list = (List<Object>) parentFE.getProperty( pt );
            list.add( value );
          }
          else
            parentFE.setProperty( pt, value );
//          System.out.println( "          | set Property " + parentFE.getId() + " " + value.getClass().getName() );
        }
        catch( TypeRegistryException e )
        {
          e.printStackTrace();
        }
        finally
        {
          xmlReader.setContentHandler( orgCH );
          // simulate end element tag
          try
          {
            orgCH.endElement( uri, localName, qName );
          }
          catch( SAXException e )
          {
            // TODO Auto-generated catch block
            e.printStackTrace();
          }
        }
      }

    };

    final DOMConstructor con = new DOMConstructor( doc, runnable );
    xmlReader.setContentHandler( con );
    // simulate startElement
    con.startElement( uri, localName, qName, atts );
  }

  public interface Itest
  {
    public void run( Node node );

  }

  public void popPT( )
  {
    m_stackPT.pop();
  }
}
