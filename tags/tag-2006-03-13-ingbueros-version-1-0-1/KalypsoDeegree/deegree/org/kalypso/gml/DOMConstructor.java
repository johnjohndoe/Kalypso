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

import java.util.Stack;
import java.util.Vector;

import org.kalypso.gml.PropertyParser.Itest;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.ProcessingInstruction;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;
import org.xml.sax.ext.LexicalHandler;

/**
 * @author doemming
 */

public class DOMConstructor implements ContentHandler, LexicalHandler
{
  public static final String XMLNS_NSURI = "http://www.w3.org/2000/xmlns/";

  private Node m_contextNode = null;

  private Stack<Node> m_contextStack;

  private Document m_factory;

  private boolean m_inCdata = false;

  private Vector<Attr> m_prefixes = null;

  private StringBuffer m_buffer = null;

  private int m_level = 0;

  private final Itest m_runnable;

  /**
   * Create new DOMConstructor instance.
   * 
   * @param factory
   *          Factory instance to be used for creating nodes.
   */
  public DOMConstructor( final Document factory, Itest runnable )
  {
    m_factory = factory;
    m_runnable = runnable;
    m_contextStack = new Stack<Node>();
  }

  public void startElement( String uri, String local, String qname, Attributes atts )
  {
    m_level++;
    flushText();

    Element elem = m_factory.createElementNS( uri, qname );
    for( int i = 0; i < atts.getLength(); i++ )
    {
      elem.setAttributeNS( atts.getURI( i ), atts.getQName( i ), atts.getValue( i ) );
    }
    if( m_prefixes != null && m_prefixes.size() > 0 )
    {
      for( int i = 0; i < m_prefixes.size(); i++ )
      {
        Attr attr = m_prefixes.elementAt( i );
        elem.setAttributeNode( attr );
      }
      m_prefixes.removeAllElements();
    }
    pushContext( elem );
  }

  public void endElement( String uri, String local, String qname )
  {
    m_level--;
    flushText();
    output( popContext() );
    if( m_level == 0 )
    {
      final Node node = getNode();
//      System.out.println(XMLHelper.toString(node));
      m_runnable.run( node );
    }
  }

  /**
   * Return created DOM node.
   */
  public Node getNode( )
  {
    flushText();
    return m_contextNode;
  }

  protected void output( Node node )
  {
    if( m_contextNode == null )
    {
      m_contextNode = node;
    }
    else
    {
      m_contextNode.appendChild( node );
    }
  }

  protected void pushContext( Node newContext )
  {
    m_contextStack.push( m_contextNode );
    m_contextNode = newContext;
  }

  protected Node popContext( )
  {
    Node ret = m_contextNode;
    m_contextNode = m_contextStack.pop();
    return ret;
  }

  protected void flushText( )
  {
    if( m_buffer == null || m_buffer.length() == 0 )
      return;
    String text = new String( m_buffer );
    if( m_inCdata )
    {
      output( m_factory.createCDATASection( text ) );
    }
    else
    {
      output( m_factory.createTextNode( text ) );
    }
    m_buffer.setLength( 0 );
  }

  // Text and CDATA section
  public void startCDATA( )
  {
    flushText();
    m_inCdata = true;
  }

  public void endCDATA( )
  {
    flushText();
    m_inCdata = false;
  }

  public void characters( char[] ch, int start, int length )
  {
    if( m_buffer == null )
      m_buffer = new StringBuffer();
    m_buffer.append( ch, start, length );
  }

  public void ignorableWhitespace( char[] ch, int start, int len )
  {
    characters( ch, start, len );
  }

  public void processingInstruction( String target, String data )
  {
    flushText();
    ProcessingInstruction pi;
    pi = m_factory.createProcessingInstruction( target, data );
    output( pi );
  }

  public void comment( char[] ch, int start, int length )
  {
    flushText();
    String data = new String( ch, start, length );
    output( m_factory.createComment( data ) );
  }

  public void startDocument( )
  {
    pushContext( m_factory );
  }

  public void endDocument( )
  {
    output( popContext() );
  }

  public void startPrefixMapping( String prefix, String uri )
  {
    if( m_prefixes == null )
      m_prefixes = new Vector<Attr>();
    else
      m_prefixes.removeAllElements();

    String qname = "xmlns";
    if( prefix.length() > 0 )
      qname = "xmlns:" + prefix;
    Attr attr = m_factory.createAttributeNS( XMLNS_NSURI, qname );
    attr.setNodeValue( uri );
    m_prefixes.addElement( attr );
  }

  public void endPrefixMapping( String prefix )
  {
  }

  // EntityReference
  public void startEntity( String name )
  {
    flushText();
    Node entityref = m_factory.createEntityReference( name );
    pushContext( entityref );
  }

  public void endEntity( String name )
  {
    flushText();
    output( popContext() );
  }

  // DOCTYPE: ignored
  public void startDTD( String root, String p, String s )
  {
  }

  public void endDTD( )
  {
  }

  public void setDocumentLocator( Locator locator )
  {
  }

  public void skippedEntity( String name )
  {
  }
}
