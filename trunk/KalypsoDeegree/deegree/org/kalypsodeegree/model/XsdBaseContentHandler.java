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
package org.kalypsodeegree.model;

import org.kalypso.contribs.java.xml.XMLUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.UnMarshallResultEater;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;

/**
 * The content handler for the {@link org.kalypsodeegree.model.XsdBaseTypeHandler}.
 * <p>
 * It combines the before used classes {@link org.kalypso.gmlschema.types.SimpleTypeUnmarshalingContentHandler},
 * {@link org.kalypso.gml.ToStringContentHandler}, internally used
 * {@link org.kalypso.gmlschema.types.UnMarshallResultEater} and internally used
 * {@link org.kalypso.contribs.java.util.logging.ILogger}.
 * </p>
 * <p>
 * The classes where combined for performance reasons and to avoid the creation of a content handler for each call to
 * {@link org.kalypsodeegree.model.XsdBaseTypeHandler#unmarshal(XMLReader, URL, UnMarshallResultEater, String)}.
 * </p>
 * <p>See also remark in {@link org.kalypsodeegree.model.XsdBaseTypeHandler#unmarshal(XMLReader, URL, UnMarshallResultEater, String)}.</p>
 * 
 * @author Gernot Belger
 */
public class XsdBaseContentHandler implements ContentHandler
{
  private final StringBuffer m_buffer = new StringBuffer();

  private UnMarshallResultEater m_marshalResultEater;

  private final IMarshallingTypeHandler m_typeHandler;

  /**
   * @param marshalResultEater
   *          will be feeded with the result of unmarshalling process
   * @param unmarshallResultProvider
   *          should provide the parsed values
   * @param unmarshallerHandler
   *          contenthandler that will be wrapped for unmarshalling
   */
  public XsdBaseContentHandler( final IMarshallingTypeHandler typeHandler, final UnMarshallResultEater marshalResultEater )
  {
    m_typeHandler = typeHandler;
    m_marshalResultEater = marshalResultEater;
  }

  /**
   * @param resetBuffer
   *          Clears the intenal string buffer
   */
  public void setMarshalResultEater( final UnMarshallResultEater marshalResultEater, final boolean resetBuffer )
  {
    m_marshalResultEater = marshalResultEater;
    if( resetBuffer == true )
      m_buffer.delete( 0, m_buffer.length() );
  }

  public void startElement( String uri, String local, String qname, Attributes atts ) throws SAXException
  {
    try
    {
      end();
    }
    catch( final GMLSchemaException e )
    {
      throw new SAXException( e );
    }
  }

  public void endElement( String uri, String local, String qname ) throws SAXException
  {
    try
    {
      end();
    }
    catch( final GMLSchemaException e )
    {
      throw new SAXException( e );
    }
  }

  private void end( ) throws GMLSchemaException
  {
    Object value = null;
    try
    {
      final String stringResult = m_buffer.toString();

      // HACK: remove CDATA section markers
      // TODO shouldn't the saxparser handle this? Check if this is ok what is done here...
      final String withoutCDATA = stringResult.replace( XMLUtilities.CDATA_BEGIN, "" ).replace( XMLUtilities.CDATA_END, "" );

      // if( !withoutCDATA.equals( stringResult ) )
      // System.out.println();

      value = m_typeHandler.parseType( withoutCDATA );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      // TODO: why no rethrow exception here?
    }
    finally
    {
      if( m_marshalResultEater != null )
        m_marshalResultEater.eat( value );
    }
  }

  public void characters( char[] ch, int start, int length )
  {
    m_buffer.append( ch, start, length );
  }

  public void ignorableWhitespace( char[] ch, int start, int len )
  {
    m_buffer.append( ch, start, len );
  }

  public void processingInstruction( String target, String data )
  {
    m_buffer.append( "processingInstruction: " + target + " / " + data + "\n" );
  }

  public void startDocument( )
  {
  }

  public void endDocument( )
  {
  }

  public void startPrefixMapping( String prefix, String uri )
  {
  }

  public void endPrefixMapping( String prefix )
  {
  }

  public void setDocumentLocator( Locator locator )
  {
  }

  public void skippedEntity( String name )
  {
    m_buffer.append( "skippedEntity: " + name + "\n" );
  }
}
