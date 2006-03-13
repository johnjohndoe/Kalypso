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
      // TODO use encloding
      final TransformerFactory tFac = TransformerFactory.newInstance();
      tFac.setAttribute( "indent-number", new Integer( 4 ) );
      final Transformer transformer = tFac.newTransformer();

      final XMLReader reader = new GMLWorkspaceReader();
      reader.setFeature( "http://xml.org/sax/features/namespaces", true );
      reader.setFeature( "http://xml.org/sax/features/namespace-prefixes", true );
      final InputSource inpuSource = new GMLWorkspaceInputSource( gmlWorkspace );
      final Source source = new SAXSource( reader, inpuSource );
      final StreamResult result = new StreamResult( writer );
      // TODO use this:
      // transformer.setOutputProperty( "{http://xml.apache.org/xalan}indent-amount", "2" );
      // t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" );
      // transformer.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "5" );
      transformer.setOutputProperty( OutputKeys.METHOD, "xml" );
      transformer.setOutputProperty( OutputKeys.INDENT, "yes" );
      transformer.transform( source, result );

      // final GMLDocument gmlDoc = new GMLDocument_Impl();
      //
      // final String schemaNamespace = workspace.getSchemaNamespace();
      // if( schemaNamespace != null )
      // {
      // final GMLNameSpace namespace = new GMLNameSpace_Impl( null, schemaNamespace );
      // gmlDoc.addNameSpace( namespace );
      // }
      //
      // final Map namespaces = workspace.getNamespaceMap();
      // for( final Iterator entryIt = namespaces.entrySet().iterator(); entryIt.hasNext(); )
      // {
      // final Map.Entry entry = (Entry) entryIt.next();
      // final GMLNameSpace_Impl ns = new GMLNameSpace_Impl( (String) entry.getKey(), (String) entry.getValue() );
      // // do not use the xmlns:xmlns namespace, its the LAW!
      // if( !ns.getSubSpaceName().equals( "xmlns" ) )
      // gmlDoc.addNameSpace( ns );
      // }
      //
      // // TODO: why aren't those already in the namespace map???
      // final GMLNameSpace gmlNameSpace = new GMLNameSpace_Impl( "gml", CommonNamespaces.GMLNS );
      // final GMLNameSpace xlinkNameSpace = new GMLNameSpace_Impl( "xlink", CommonNamespaces.XLINKNS );
      // final GMLNameSpace xsiNameSpace = new GMLNameSpace_Impl( "xsi", CommonNamespaces.XSINS );
      // gmlDoc.addNameSpace( gmlNameSpace );
      // gmlDoc.addNameSpace( xlinkNameSpace );
      // gmlDoc.addNameSpace( xsiNameSpace );
      //
      // final GMLFeature gmlFeature = GMLFactory.createGMLFeature( gmlDoc, workspace.getRootFeature(),
      // workspace.getContext() );
      // gmlDoc.setRoot( gmlFeature );
      //
      // workspace.getContext();
      // final String schemaLoc = workspace.getSchemaLocation();
      // if( schemaLoc != null )
      // gmlDoc.setSchemaLocation( schemaNamespace + " " + schemaLoc );
      //
      // XMLHelper.writeDOM( gmlDoc, charsetEncoding, writer );
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

      return createGMLWorkspace( new InputSource( stream ), gmlURL, null );
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

      return createGMLWorkspace( new InputSource( rt ), gmlURL, urlResolver );
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  private static GMLWorkspace createGMLWorkspace( final InputSource inputSource, final URL context, final IUrlResolver urlResolver ) throws Exception, GmlSerializeException
  {
    final SAXParserFactory saxFac = SAXParserFactory.newInstance();
    saxFac.setNamespaceAware( true );
    final SAXParser saxParser = saxFac.newSAXParser();
    final XMLReader xmlReader = saxParser.getXMLReader();
    final GMLContentHandler contentHandler = new GMLContentHandler( xmlReader );
    xmlReader.setContentHandler( contentHandler );
    xmlReader.parse( inputSource );

    final GMLSchema schema = contentHandler.getGMLSchema();
    final Feature rootFeature = contentHandler.getRootFeature();
    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( schema, rootFeature, context, null );
    return workspace;

    //    
    // final Document gmlAsDOM = XMLHelper.getAsDOM( inputSource, true );
    // final GMLDocument_Impl gml = new GMLDocument_Impl( gmlAsDOM );
    //
    // final GMLSchema schema = loadSchemaForGmlDoc( context, gml );
    //
    // return createGMLWorkspace( gml, schema, context, urlResolver );
  }

  // private static GMLWorkspace createGMLWorkspace( final GMLDocument_Impl gml, final GMLSchema schema, final URL
  // context, final IUrlResolver urlResolver ) throws Exception
  // {
  // // create feature and workspace gml
  // final IFeatureType[] types = schema.getAllFeatureTypes();
  // final Feature feature = FeatureFactory.createFeature( gml.getRootFeature(), types, context, urlResolver );
  //
  // return new GMLWorkspace_Impl( schema, types, feature, context, gml.getSchemaLocationName() );
  // }

  // /**
  // * Lädt ein schema anhand des gml-doc. Immer aus dem Cache. Zuerst per Namespace, dann per schemaLocation.
  // *
  // * @param context
  // * context to resolve relative urls, or <code>null</code> if context unknown
  // */
  // private static GMLSchema loadSchemaForGmlDoc( final URL context, final GMLDocument gmldoc ) throws
  // GmlSerializeException
  // {
  // final String schemaURI = gmldoc.getDocumentElement().getNamespaceURI();
  // final GMLSchema schema = GMLSchemaCatalog.getSchema( schemaURI );
  // if( schema == null )
  // {
  // StringBuffer errorMessage = new StringBuffer( ". Noch über die SchemaLocation: " );
  //
  // try
  // {
  // final URL schemaLocation = gmldoc.getSchemaLocation( context );
  // final GMLSchema schema2 = GMLSchemaCatalog.getSchema( schemaLocation );
  //
  // if( schema2 != null )
  // return schema2;
  //
  // errorMessage.append( schemaLocation );
  // }
  // catch( final MalformedURLException e )
  // {
  // errorMessage.append( e.getLocalizedMessage() ).append( ". Häufige Ursache ist ein fehlendes Schema im Cache
  // (Kalypso-Server steht nicht zur Verfügung bzw. liefert nicht das notwendige Schema?)" );
  //
  // Logger.getLogger( GmlSerializer.class.getName() ).warning( errorMessage.toString() );
  // }
  //
  // throw new GmlSerializeException( "GML-Schema konnte nicht geladen werden. Weder über den Namespace: " + schemaURI +
  // errorMessage );
  // }
  //
  // return schema;
  // }

  /**
   * crrate GMLWorkspace from inputStream and GMLSchema<br>
   * intended to be used from WFSLoader, where the GMLSchema can not be loaded from normal URLCatalog<br>
   * otherwise use other methode !
   */
  public static GMLWorkspace createGMLWorkspace( final InputStream inputStream, final GMLSchema gmlSchema )
  {
    throw new UnsupportedOperationException();
  }
  // /**
  // * @deprecated does not use THE CACHE
  // */
  // public static GMLWorkspace createGMLWorkspace( final InputStream inputStream, final URL schemaURL ) throws
  // Exception
  // {
  // final Document gmlAsDOM = XMLHelper.getAsDOM( new InputSource( inputStream ), true );
  // final GMLDocument_Impl gml = new GMLDocument_Impl( gmlAsDOM );
  //
  // GMLSchema schema = null;
  // if( schemaURL != null )
  // schema = GMLSchemaFactory.createGMLSchema( schemaURL );
  // else
  // // TODO load multiple Schema from schemaLocation -> Feature is composed of featureTypes from different schemas!!!
  // schema = loadSchemaForGmlDoc( null, gml );
  // return createGMLWorkspace( gml, schema, schemaURL, null );
  // }
}