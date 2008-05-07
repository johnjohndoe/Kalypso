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
package org.kalypso.ogc.gml.wms.deegree.document;

import java.io.IOException;
import java.io.PushbackReader;
import java.io.Reader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.deegree.ogcwebservices.wms.capabilities.WMSCapabilitiesDocument;
import org.kalypso.i18n.Messages;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * Fix for loading xml documents.
 * 
 * @author Holger Albert
 */
public class KalypsoWMSCapabilitiesDocument extends WMSCapabilitiesDocument
{
  /**
   * Initializes the <code>XMLFragment</code> with the content from the given <code>Reader</code>. Sets the
   * SystemId, too.
   * 
   * @param reader
   * @param systemId
   *            can not be null. This string should represent a URL that is related to the passed reader. If this URL is
   *            not available or unknown, the string should contain the value of XMLFragment.DEFAULT_URL
   * @throws SAXException
   * @throws IOException
   * @throws NullPointerException
   */
  @Override
  public void load( Reader reader, String systemId ) throws SAXException, IOException
  {
    PushbackReader pbr = new PushbackReader( reader, 1024 );
    int c = pbr.read();
    if( c != 65279 && c != 65534 )
    {
      // no BOM! push char back into reader
      pbr.unread( c );
    }

    InputSource source = new InputSource( pbr );
    if( systemId == null )
    {
      throw new NullPointerException( Messages.getString("org.kalypso.ogc.gml.wms.deegree.document.KalypsoWMSCapabilitiesDocument.0") ); //$NON-NLS-1$
    }
    setSystemId( systemId );
    DocumentBuilder builder = getDocumentBuilder();

    Document doc = builder.parse( source );
    setRootElement( doc.getDocumentElement() );
  }

  /**
   * Create a new document builder with:
   * <UL>
   * <li>namespace awareness = true
   * <li>whitespace ignoring = false
   * <li>validating = false
   * <li>expandind entity references = false
   * </UL>
   * 
   * @return new document builder
   */
  public synchronized DocumentBuilder getDocumentBuilder( )
  {
    DocumentBuilder builder = null;
    try
    {
      DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
      factory.setNamespaceAware( true );
      factory.setExpandEntityReferences( false );
      factory.setIgnoringElementContentWhitespace( false );
      factory.setValidating( false );
      // factory.setAttribute( "http://xml.org/sax/features/validation", Boolean.FALSE );
      // factory.setAttribute( "http://apache.org/xml/features/nonvalidating/load-dtd-grammar", Boolean.FALSE );
      // factory.setAttribute( "http://apache.org/xml/features/nonvalidating/load-external-dtd", Boolean.FALSE );
      // factory.setAttribute( "http://apache.org/xml/features/validation/schema", Boolean.FALSE );
      // factory.setAttribute( "http://xml.org/sax/features/external-general-entities", Boolean.FALSE );
      // factory.setAttribute( "http://xml.org/sax/features/external-parameter-entities", Boolean.FALSE );

      builder = factory.newDocumentBuilder();
      builder.setEntityResolver( new KalypsoWMSEntityResolver() );
    }
    catch( Exception ex )
    {
      ex.printStackTrace();
    }

    return builder;
  }
}