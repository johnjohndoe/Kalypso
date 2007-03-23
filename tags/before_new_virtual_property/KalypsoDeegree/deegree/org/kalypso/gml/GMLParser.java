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

import java.io.IOException;
import java.io.OutputStream;
import java.net.URL;
import java.util.HashMap;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamResult;

import org.kalypso.gmlschema.GMLSchema;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.XMLReader;

/**
 * @author doemming
 * @deprecated do not use anymore
 * @see GMLSerializer
 */
@Deprecated
public class GMLParser
{
  public GMLWorkspace parseGML( final URL gmlURL ) throws SAXException, ParserConfigurationException, IOException, GMLException
  {
    final SAXParserFactory saxFac = SAXParserFactory.newInstance();
    saxFac.setNamespaceAware( true );
    final SAXParser saxParser = saxFac.newSAXParser();
    final XMLReader xmlReader = saxParser.getXMLReader();
    final GMLContentHandler contentHandler = new GMLContentHandler( xmlReader, gmlURL );
    xmlReader.setContentHandler( contentHandler );
    final InputSource source = new InputSource( gmlURL.openStream() );
    xmlReader.parse( source );

    final GMLSchema schema = contentHandler.getGMLSchema();

    final Feature rootFeature = contentHandler.getRootFeature();
    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( schema, rootFeature, gmlURL, null, null );
    return workspace;
  }

  public void writeGML( final GMLWorkspace gmlWorkspace, final OutputStream outputStream ) throws TransformerException, SAXNotRecognizedException, SAXNotSupportedException
  {

    final TransformerFactory tFac = TransformerFactory.newInstance();
    tFac.setAttribute( "indent-number", new Integer( 4 ) );
    final Transformer transformer = tFac.newTransformer();

    final XMLReader reader = new GMLWorkspaceReader( new HashMap<String, String>() );
    reader.setFeature( "http://xml.org/sax/features/namespaces", true );
    reader.setFeature( "http://xml.org/sax/features/namespace-prefixes", true );
    final InputSource inpuSource = new GMLWorkspaceInputSource( gmlWorkspace );
    final Source source = new SAXSource( reader, inpuSource );
    final StreamResult result = new StreamResult( outputStream );
    // transformer.setOutputProperty(
    // "{http://xml.apache.org/xalan}indent-amount", "2" );
    // t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2"
    // );
    // transformer.setOutputProperty(
    // "{http://xml.apache.org/xslt}indent-amount", "5" );
    transformer.setOutputProperty( OutputKeys.METHOD, "xml" );
    transformer.setOutputProperty( OutputKeys.INDENT, "yes" );
    transformer.transform( source, result );

  }

}
