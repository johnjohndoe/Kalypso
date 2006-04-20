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

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;

/**
 * @author doemming
 */

public class BindingUnmarshalingContentHandler implements ContentHandler
{
  public static final String XMLNS_NSURI = "http://www.w3.org/2000/xmlns/";

  private int m_level = 0;

  private final ContentHandler m_unmarshallerHandler;

  private final Runnable m_runnable;

  public BindingUnmarshalingContentHandler( ContentHandler unmarshallerHandler, Runnable runnable )
  {
    m_unmarshallerHandler = unmarshallerHandler;
    m_runnable = runnable;
  }

  public void startElement( String uri, String local, String qname, Attributes atts ) throws SAXException
  {
    m_level++;
    m_unmarshallerHandler.startElement( uri, local, qname, atts );
  }

  public void endElement( String uri, String local, String qname ) throws SAXException
  {
    m_level--;
    m_unmarshallerHandler.endElement( uri, local, qname );
    if( m_level == 0 )
      m_runnable.run();
  }

  public void characters( char[] ch, int start, int length ) throws SAXException
  {
    m_unmarshallerHandler.characters( ch, start, length );
  }

  public void ignorableWhitespace( char[] ch, int start, int len ) throws SAXException
  {
    m_unmarshallerHandler.ignorableWhitespace( ch, start, len );
  }

  public void processingInstruction( String target, String data ) throws SAXException
  {
    m_unmarshallerHandler.processingInstruction( target, data );
  }

  public void startDocument( ) throws SAXException
  {
    m_unmarshallerHandler.startDocument();
  }

  public void endDocument( ) throws SAXException
  {
    m_unmarshallerHandler.endDocument();
  }

  public void startPrefixMapping( String prefix, String uri ) throws SAXException
  {
    m_unmarshallerHandler.startPrefixMapping( prefix, uri );
  }

  public void endPrefixMapping( String prefix ) throws SAXException
  {
    m_unmarshallerHandler.endPrefixMapping( prefix );
  }

  public void setDocumentLocator( Locator locator )
  {
    m_unmarshallerHandler.setDocumentLocator( locator );
  }

  public void skippedEntity( String name ) throws SAXException
  {
    m_unmarshallerHandler.skippedEntity( name );
  }
}
