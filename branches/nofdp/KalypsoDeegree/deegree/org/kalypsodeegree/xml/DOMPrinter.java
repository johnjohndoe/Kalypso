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
package org.kalypsodeegree.xml;

import java.io.PrintWriter;

import org.kalypsodeegree_impl.tools.StringExtend;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class DOMPrinter
{

  public static void printNode( PrintWriter out, Node node )
  {
    switch( node.getNodeType() )
    {
    case Node.DOCUMENT_NODE:
    {
      out.print( "<?xml version=\"1.0\"?>" );
      Document doc = (Document)node;
      printNode( out, doc.getDocumentElement() );
      break;
    }
    case Node.ELEMENT_NODE:
    {
      String name = node.getNodeName();
      out.print( "<" + name );
      NamedNodeMap attributes = node.getAttributes();
      for( int i = 0; i < attributes.getLength(); i++ )
      {
        Node current = attributes.item( i );
        String value = current.getNodeValue();
        value = StringExtend.replace( value, "&", "&amp;", true );
        out.print( " " + current.getNodeName() + "=\"" + value + "\"" );
      }
      out.print( ">" );

      // Kinder durchgehen
      NodeList children = node.getChildNodes();
      if( children != null )
      {
        for( int i = 0; i < children.getLength(); i++ )
        {
          printNode( out, children.item( i ) );
        }
      }

      out.print( "</" + name + ">" );
      break;
    }
    case Node.TEXT_NODE:
    case Node.CDATA_SECTION_NODE:
    {
      String trimmed = node.getNodeValue().trim();
      if( !trimmed.equals( "" ) )
        out.print( XMLTools.validateCDATA( trimmed ) );
      break;
    }
    case Node.PROCESSING_INSTRUCTION_NODE:
    {
      break;
    }
    case Node.ENTITY_REFERENCE_NODE:
    {
      break;
    }
    case Node.DOCUMENT_TYPE_NODE:
    {
      break;
    }
    }
  }

  public static void printNode( Node node, String indent )
  {
    switch( node.getNodeType() )
    {
    case Node.DOCUMENT_NODE:
    {
      System.out.println( "<?xml version=\"1.0\"?>" );
      Document doc = (Document)node;
      printNode( doc.getDocumentElement(), "" );
      break;
    }
    case Node.ELEMENT_NODE:
    {
      String name = node.getNodeName();
      System.out.print( indent + "<" + name );
      NamedNodeMap attributes = node.getAttributes();
      for( int i = 0; i < attributes.getLength(); i++ )
      {
        Node current = attributes.item( i );
        String value = current.getNodeValue();
        value = StringExtend.replace( value, "&", "&amp;", true );
        System.out.print( " " + current.getNodeName() + "=\"" + value + "\"" );
      }
      System.out.println( ">" );

      // Kinder durchgehen
      NodeList children = node.getChildNodes();
      if( children != null )
      {
        for( int i = 0; i < children.getLength(); i++ )
        {
          printNode( children.item( i ), indent + "  " );
        }
      }

      System.out.println( indent + "</" + name + ">" );
      break;
    }
    case Node.TEXT_NODE:
    case Node.CDATA_SECTION_NODE:
    {
      String trimmed = node.getNodeValue().trim();
      if( !trimmed.equals( "" ) )
        System.out.println( indent + "<![CDATA[" + trimmed + "]]>" );
      break;
    }
    case Node.PROCESSING_INSTRUCTION_NODE:
    {
      break;
    }
    case Node.ENTITY_REFERENCE_NODE:
    {
      break;
    }
    case Node.DOCUMENT_TYPE_NODE:
    {
      break;
    }
    }
  }

  public static String nodeToString( Node node, String encoding )
  {
    StringBuffer sb = new StringBuffer( 10000 );

    switch( node.getNodeType() )
    {
    case Node.DOCUMENT_NODE:
    {
      sb.append( "<?xml version=\"1.0\" encoding=\"" + encoding + "\" ?>" );
      Document doc = (Document)node;
      sb.append( nodeToString( doc.getDocumentElement(), "" ) );
      break;
    }
    case Node.ELEMENT_NODE:
    {
      String name = node.getNodeName();
      sb.append( "\n<" + name );
      NamedNodeMap attributes = node.getAttributes();
      for( int i = 0; i < attributes.getLength(); i++ )
      {
        Node current = attributes.item( i );
        String value = current.getNodeValue();
        value = StringExtend.replace( value, "&", "&amp;", true );
        sb.append( " " + current.getNodeName() + "=\"" + value + "\"" );
      }
      sb.append( ">" );

      // Kinder durchgehen
      NodeList children = node.getChildNodes();
      if( children != null )
      {
        for( int i = 0; i < children.getLength(); i++ )
        {
          sb.append( nodeToString( children.item( i ), encoding ) );
        }
      }

      sb.append( "</" + name + ">" );
      break;
    }
    case Node.CDATA_SECTION_NODE:
    {
      String trimmed = node.getNodeValue().trim();
      if( !trimmed.equals( "" ) )
        sb.append( "<![CDATA[" + trimmed + "]]>" );
      break;
    }
    case Node.TEXT_NODE:
    {
      String trimmed = node.getNodeValue().trim();
      if( !trimmed.equals( "" ) )
      {
        sb.append( XMLTools.validateCDATA( trimmed ) );
      }
      break;
    }
    case Node.PROCESSING_INSTRUCTION_NODE:
    {
      break;
    }
    case Node.ENTITY_REFERENCE_NODE:
    {
      break;
    }
    case Node.DOCUMENT_TYPE_NODE:
    {
      break;
    }
    }
    return sb.toString();
  }

}