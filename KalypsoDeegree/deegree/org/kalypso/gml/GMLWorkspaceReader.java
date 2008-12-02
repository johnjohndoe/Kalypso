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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kalypso.contribs.org.xml.sax.IndentingContentHandler;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.xml.sax.ContentHandler;
import org.xml.sax.DTDHandler;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

/**
 * @author doemming
 */
public class GMLWorkspaceReader implements XMLReader
{
  private final List<String> m_enabledFeatures = new ArrayList<String>();

  private final HashMap<String, Object> m_propMap = new HashMap<String, Object>();

  private final Map<String, String> m_idMap;

  private EntityResolver m_entityResolver;

  private DTDHandler m_dtdHandler;

  private ContentHandler m_contentHandler;

  private ErrorHandler m_errorHandler;

  /**
   * @param idMap
   *            (existing-ID,new-ID) mapping for ids, replace all given Ids in GML (feature-ID and links)
   */
  public GMLWorkspaceReader( final Map<String, String> idMap )
  {
    m_idMap = idMap;
  }

  /**
   * @see org.xml.sax.XMLReader#parse(org.xml.sax.InputSource)
   */
  public void parse( final InputSource input ) throws SAXException
  {
    if( input == null || !(input instanceof GMLWorkspaceInputSource) )
      throw new SAXException( "inputSource is null or not of type: " + GMLWorkspaceInputSource.class.getName() );

    final ContentHandler handler = getContentHandler();
    final GMLWorkspace workspace = ((GMLWorkspaceInputSource) input).getGMLWorkspace();

    // Bit of a hack: wrap content handler in order to support indentation
    final IndentingContentHandler indentHandler = new IndentingContentHandler( handler, 1 );
    setContentHandler( indentHandler );

    final GMLSAXFactory factory = new GMLSAXFactory( this, m_idMap );

    handler.startDocument();
    factory.process( workspace );
    handler.endDocument();
  }

  public boolean getFeature( final String name )
  {
    return m_enabledFeatures.contains( name );
  }

  /**
   * @see org.xml.sax.XMLReader#setFeature(java.lang.String, boolean)
   */
  public void setFeature( final String name, final boolean value )
  {
    if( value )
      m_enabledFeatures.add( name );
    else
      m_enabledFeatures.remove( name );
  }

  /**
   * @see org.xml.sax.XMLReader#getProperty(java.lang.String)
   */
  public Object getProperty( final String name )
  {
    return m_propMap.get( name );
  }

  /**
   * @see org.xml.sax.XMLReader#setProperty(java.lang.String, java.lang.Object)
   */
  public void setProperty( final String name, final Object value )
  {
    m_propMap.put( name, value );
  }

  /**
   * @see org.xml.sax.XMLReader#setEntityResolver(org.xml.sax.EntityResolver)
   */
  public void setEntityResolver( final EntityResolver resolver )
  {
    m_entityResolver = resolver;
  }

  /**
   * @see org.xml.sax.XMLReader#getEntityResolver()
   */
  public EntityResolver getEntityResolver( )
  {
    return m_entityResolver;
  }

  /**
   * @see org.xml.sax.XMLReader#setDTDHandler(org.xml.sax.DTDHandler)
   */
  public void setDTDHandler( final DTDHandler handler )
  {
    m_dtdHandler = handler;
  }

  /**
   * @see org.xml.sax.XMLReader#getDTDHandler()
   */
  public DTDHandler getDTDHandler( )
  {
    return m_dtdHandler;
  }

  /**
   * @see org.xml.sax.XMLReader#setContentHandler(org.xml.sax.ContentHandler)
   */
  public void setContentHandler( final ContentHandler handler )
  {
    m_contentHandler = handler;
  }

  /**
   * @see org.xml.sax.XMLReader#getContentHandler()
   */
  public ContentHandler getContentHandler( )
  {
    return m_contentHandler;
  }

  /**
   * @see org.xml.sax.XMLReader#setErrorHandler(org.xml.sax.ErrorHandler)
   */
  public void setErrorHandler( final ErrorHandler handler )
  {
    m_errorHandler = handler;
  }

  /**
   * @see org.xml.sax.XMLReader#getErrorHandler()
   */
  public ErrorHandler getErrorHandler( )
  {
    return m_errorHandler;
  }

  /**
   * @see org.xml.sax.XMLReader#parse(java.lang.String)
   */
  public void parse( final String systemId )
  {
    throw new UnsupportedOperationException();
  }

}
