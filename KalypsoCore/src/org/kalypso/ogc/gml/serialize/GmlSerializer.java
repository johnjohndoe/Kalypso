/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.serialize;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.net.URL;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.IOUtils;
import org.apache.tools.ant.filters.ReplaceTokens;
import org.apache.tools.ant.filters.ReplaceTokens.Token;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.gml.GMLContentHandler;
import org.kalypso.gml.GMLWorkspaceInputSource;
import org.kalypso.gml.GMLWorkspaceReader;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.xml.sax.InputSource;
import org.xml.sax.XMLReader;

/**
 * Helper - Klasse, um Gml zu lesen und zu schreiben
 * 
 * @author Belger
 */
public final class GmlSerializer
{

  private GmlSerializer( )
  {
    // do not instantiate this class
  }

  public static void serializeWorkspace( final OutputStreamWriter writer, final GMLWorkspace workspace ) throws GmlSerializeException
  {
    serializeWorkspace( writer, workspace, writer.getEncoding() );
  }

  public static void serializeWorkspace( final Writer writer, final GMLWorkspace gmlWorkspace, final String charsetEncoding ) throws GmlSerializeException
  {
    try
    {
      final XMLReader reader = new GMLWorkspaceReader();
      reader.setFeature( "http://xml.org/sax/features/namespaces", true );
      reader.setFeature( "http://xml.org/sax/features/namespace-prefixes", true );
      
      final InputSource inputSource = new GMLWorkspaceInputSource( gmlWorkspace );
      inputSource.setEncoding( charsetEncoding );
      
      final Source source = new SAXSource( reader, inputSource );
      final StreamResult result = new StreamResult( writer );

      final TransformerFactory tFac = TransformerFactory.newInstance();
      final Transformer transformer = tFac.newTransformer();
      transformer.setOutputProperty( OutputKeys.ENCODING, charsetEncoding );
      transformer.setOutputProperty( OutputKeys.INDENT, "yes" );
      transformer.setOutputProperty( OutputKeys.METHOD, "xml" );
      transformer.transform( source, result );
    }
    catch( final Exception e )
    {
      throw new GmlSerializeException( "Fehler beim Schreiben des GML Stream", e );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  /**
   * Liest einen GML-Workspace aus einer URL. Es wird kein Token-Replace durchgeführt, das Encoding wird anhand des
   * XML-Headers ermittelt. Sollte Client-Seitig nicht benutzt werden.
   */
  public static GMLWorkspace createGMLWorkspace( final URL gmlURL ) throws Exception
  {
    InputStream stream = null;
    try
    {
      // Besser streams benutzen, da falls das encoding im reader nicht bekannt
      // ist garantiert Mist rauskommt
      // der XML Mechanismus decodiert so schon richtig, zumindest, wenn das
      // richtige enconding im xml-header steht.
      stream = new BufferedInputStream( gmlURL.openStream() );

      return createGMLWorkspace( new InputSource( stream ), gmlURL );
    }
    finally
    {
      IOUtils.closeQuietly( stream );
    }
  }

  /**
   * Liest ein GML aus einer URL und ersetzt dabei tokens gemäss dem URL-Resolver.
   */
  public static GMLWorkspace createGMLWorkspace( final URL gmlURL, final IUrlResolver urlResolver ) throws Exception
  {
    Reader reader = null;

    try
    {
      final InputStreamReader isr = urlResolver.createReader( gmlURL );
      if( isr.getEncoding() == null )
      {
        IOUtils.closeQuietly( isr );
        throw new NullPointerException( "Es konnte kein Encoding für die GMLUrl ermittelt werden. Dies sollte auf Client-Seite eigentlich nie passieren. Serverseitig darf diese Methode nicht benutzt werden." );
      }

      reader = new BufferedReader( isr );
      // Replace tokens
      final ReplaceTokens rt = new ReplaceTokens( reader );
      rt.setBeginToken( ':' );
      rt.setEndToken( ':' );
      for( final Iterator tokenIt = urlResolver.getReplaceEntries(); tokenIt.hasNext(); )
      {
        final Map.Entry entry = (Entry) tokenIt.next();

        final Token token = new ReplaceTokens.Token();
        token.setKey( (String) entry.getKey() );
        token.setValue( (String) entry.getValue() );

        rt.addConfiguredToken( token );
      }

      return createGMLWorkspace( new InputSource( rt ), gmlURL );
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  private static GMLWorkspace createGMLWorkspace( final InputSource inputSource, final URL context ) throws Exception
  {
    final SAXParserFactory saxFac = SAXParserFactory.newInstance();
    saxFac.setNamespaceAware( true );

    final SAXParser saxParser = saxFac.newSAXParser();
    final XMLReader xmlReader = saxParser.getXMLReader();
    final GMLContentHandler contentHandler = new GMLContentHandler( xmlReader, context );
    xmlReader.setContentHandler( contentHandler );
    xmlReader.parse( inputSource );

    final GMLSchema schema = contentHandler.getGMLSchema();
    final Feature rootFeature = contentHandler.getRootFeature();
    final String schemaLocationString = contentHandler.getSchemaLocationString();
    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( schema, rootFeature, context, schemaLocationString );
    return workspace;
  }

  public static GMLWorkspace createGMLWorkspace( final BufferedInputStream inputStream, final URL schemaURLHint, final boolean useGMLSchemaCache ) throws Exception
  {
    final SAXParserFactory saxFac = SAXParserFactory.newInstance();
    saxFac.setNamespaceAware( true );
    final SAXParser saxParser = saxFac.newSAXParser();
    final XMLReader xmlReader = saxParser.getXMLReader();
    final GMLContentHandler contentHandler = new GMLContentHandler( xmlReader, schemaURLHint, useGMLSchemaCache, null );
    xmlReader.setContentHandler( contentHandler );
    xmlReader.parse( new InputSource( inputStream ) );
    final GMLSchema schema = contentHandler.getGMLSchema();
    final Feature rootFeature = contentHandler.getRootFeature();
    final String schemaLocationString = contentHandler.getSchemaLocationString();
    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( schema, rootFeature, null, schemaLocationString );
    return workspace;
  }
}