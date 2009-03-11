/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import java.net.URL;

import org.kalypso.contribs.org.xml.sax.AppendingContentHandler;
import org.kalypso.contribs.org.xml.sax.DelegateContentHandler;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

/**
 * A content handler that parses a response of a WFS request.
 * <p>
 * If the request is gml, all parsing is delegated to the {@link GMLContentHandler}.
 * </p>
 * <p>
 * If the request is an exception, justs logs the content into a string and throws it as an GmlException.
 * </p>
 * 
 * @author Gernot Belger
 * @author Andreas von Dömming
 */
public class GMLorExceptionContentHandler extends DelegateContentHandler
{
  private int m_depth = 0;

  public GMLorExceptionContentHandler( final XMLReader xmlReader, final URL schemaLocationHint, final URL context, final IFeatureProviderFactory providerFactory )
  {
    super( new GMLDocumentContentHandler( xmlReader, schemaLocationHint, context, providerFactory ) );
  }

  /**
   * If the first element of the document is the exception element, changes the delegate.
   * 
   * @see org.xml.sax.ContentHandler#startElement(java.lang.String, java.lang.String, java.lang.String,
   *      org.xml.sax.Attributes)
   */
  @Override
  public void startElement( final String uri, final String localName, final String qName, final Attributes atts ) throws SAXException
  {
    // handle OGC Exceptions
    // Handle degree1 + deegree2 exepctions.
    // deegree1-service: ...Exception
    // deegree2-service: ServiceExceptionReport
    // TODO: we should test for the namespace, what happens if we ever have a gml with root element 'exception'?
    if( m_depth == 0 && localName != null && (localName.endsWith( "Exception" ) || localName.equals( "ServiceExceptionReport" )) )
      setDelegate( new AppendingContentHandler( new StringBuffer() ) );

    m_depth++;

    super.startElement( uri, localName, qName, atts );
  }

  /**
   * @see org.xml.sax.ContentHandler#endElement(java.lang.String, java.lang.String, java.lang.String)
   */
  @Override
  public void endElement( final String uri, final String localName, final String qName ) throws SAXException
  {
    super.endElement( uri, localName, qName );

    m_depth--;
  }

  public GMLWorkspace getWorkspace( ) throws GMLException
  {
    final ContentHandler delegate = getDelegate();
    if( delegate instanceof AppendingContentHandler )
    {
      final Appendable buffer = ((AppendingContentHandler) delegate).getAppendable();
      throw new GMLException( buffer.toString() );
    }

    return ((GMLDocumentContentHandler) delegate).getWorkspace();
  }
}
