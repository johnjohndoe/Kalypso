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
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.Writer;
import java.net.URL;
import java.net.URLConnection;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.IOUtils;
import org.apache.tools.ant.filters.ReplaceTokens;
import org.apache.tools.ant.filters.ReplaceTokens.Token;
import org.kalypso.java.net.IUrlResolver;
import org.kalypsodeegree.gml.GMLDocument;
import org.kalypsodeegree.gml.GMLFeature;
import org.kalypsodeegree.gml.GMLNameSpace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.ogcbasic.CommonNamespaces;
import org.kalypsodeegree_impl.gml.GMLDocument_Impl;
import org.kalypsodeegree_impl.gml.GMLFactory;
import org.kalypsodeegree_impl.gml.GMLNameSpace_Impl;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCache;
import org.kalypsodeegree_impl.gml.schema.XMLHelper;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;

/**
 * Helper - Klasse, um Gml zu lesen und zu schreiben
 * 
 * @author Belger
 */
public final class GmlSerializer
{
  private GmlSerializer()
  {
  // do not instantiate this class
  }

  public static void serializeWorkspace( final Writer writer, final GMLWorkspace workspace )
      throws GmlSerializeException
  {
    serializeWorkspace( writer, workspace, null );
  }

  public static void serializeWorkspace( final Writer writer, final GMLWorkspace workspace,
      final String charsetEncoding ) throws GmlSerializeException
  {
    try
    {
      final GMLDocument gmlDoc = new GMLDocument_Impl();

      final String schemaNamespace = workspace.getSchemaNamespace();
      if( schemaNamespace != null )
      {
        final GMLNameSpace namespace = new GMLNameSpace_Impl( null, schemaNamespace );
        gmlDoc.addNameSpace( namespace );
      }

      final Map namespaces = workspace.getNamespaceMap();
      for( final Iterator entryIt = namespaces.entrySet().iterator(); entryIt.hasNext(); )
      {
        final Map.Entry entry = (Entry)entryIt.next();
        final GMLNameSpace_Impl ns = new GMLNameSpace_Impl( (String)entry.getKey(), (String)entry
            .getValue() );
        // do not use the xmlns:xmlns namespace, its the LAW!
        if( !ns.getSubSpaceName().equals( "xmlns" ) )
          gmlDoc.addNameSpace( ns );
      }

      // TODO: why aren't those already in the namespace map???
      final GMLNameSpace gmlNameSpace = new GMLNameSpace_Impl( "gml", CommonNamespaces.GMLNS );
      final GMLNameSpace xlinkNameSpace = new GMLNameSpace_Impl( "xlink", CommonNamespaces.XLINKNS );
      final GMLNameSpace xsiNameSpace = new GMLNameSpace_Impl( "xsi", CommonNamespaces.XSINS );
      gmlDoc.addNameSpace( gmlNameSpace );
      gmlDoc.addNameSpace( xlinkNameSpace );
      gmlDoc.addNameSpace( xsiNameSpace );

      final GMLFeature gmlFeature = GMLFactory
          .createGMLFeature( gmlDoc, workspace.getRootFeature() );
      gmlDoc.setRoot( gmlFeature );

      workspace.getContext();
      final String schemaLoc = workspace.getSchemaLocation();
      if( schemaLoc != null )
        gmlDoc.setSchemaLocation( schemaNamespace + " " + schemaLoc );

      // DOM als GML schreiben
      final Document xmlDOM = gmlDoc;
      final TransformerFactory newInstance = TransformerFactory.newInstance();

      final Transformer t = newInstance.newTransformer();

      t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" );
      t.setOutputProperty( OutputKeys.INDENT, "yes" );
      if( charsetEncoding != null )
        t.setOutputProperty( OutputKeys.ENCODING , charsetEncoding );

      t.transform( new DOMSource( xmlDOM ), new StreamResult( writer ) );
    }
    catch( final Exception e )
    {
      throw new GmlSerializeException( "Fehler beim Schreiben des GML Stream", e );
    }
  }

  public static GMLWorkspace createGMLWorkspace( final URL gmlURL, final URL schemaURL )
      throws Exception
  {
    // load gml
    final GMLDocument_Impl gml = new GMLDocument_Impl( XMLHelper.getAsDOM( gmlURL, true ) );

    final GMLFeature gmlFeature = gml.getRootFeature();

    // load schema
    final GMLSchema schema = GMLSchemaCache.getSchema( schemaURL );

    // create feature and workspace gml
    final FeatureType[] types = schema.getFeatureTypes();
    final Feature feature = FeatureFactory.createFeature( gmlFeature, types );

    // nicht die echte URL der schemaLocation, sondern dass, was im gml steht!
    final String schemaLocationName = gml.getSchemaLocationName();

    return new GMLWorkspace_Impl( types, feature, gmlURL, schemaLocationName, schema.getTargetNS(),
        schema.getNamespaceMap() );
  }

  public static GMLWorkspace createGMLWorkspace( final URL gmlURL, final IUrlResolver urlResolver )
      throws Exception
  {
    final URLConnection connection = gmlURL.openConnection();
    final String contentEncoding = connection.getContentEncoding();

    InputStream inputStream = null;
    try
    {
      inputStream = new BufferedInputStream( connection.getInputStream() );
      final InputStreamReader isr = contentEncoding == null ? new InputStreamReader( inputStream )
          : new InputStreamReader( inputStream, contentEncoding );

      return createGMLWorkspace( isr, urlResolver, gmlURL );
    }
    finally
    {
    IOUtils.closeQuietly( inputStream );  
    }
  }

  public static GMLWorkspace createGMLWorkspace( final Reader gmlreader, final IUrlResolver urlResolver, final URL context )
      throws Exception
  {
    // Replace tokens
    final ReplaceTokens rt = new ReplaceTokens( gmlreader );
    rt.setBeginToken( ':' );
    rt.setEndToken( ':' );
    for( final Iterator tokenIt = urlResolver.getReplaceEntries(); tokenIt.hasNext(); )
    {
      final Map.Entry entry = (Entry)tokenIt.next();

      final Token token = new ReplaceTokens.Token();
      token.setKey( (String)entry.getKey() );
      token.setValue( (String)entry.getValue() );

      rt.addConfiguredToken( token );
    }

    // load gml
    final InputSource inputSource = new InputSource( rt );
    final Document gmlAsDOM = XMLHelper.getAsDOM( inputSource, true );

    final GMLDocument_Impl gml = new GMLDocument_Impl( gmlAsDOM );

    // load schema
    final String schemaLocationName = gml.getSchemaLocationName();
    if( schemaLocationName == null || schemaLocationName.length() == 0 )
      throw new Exception( "Keine 'schemaLocation' in gml spezifiziert." );

    final URL schemaLocation = urlResolver.resolveURL( context, schemaLocationName );
    final GMLSchema schema = GMLSchemaCache.getSchema( schemaLocation );

    // create feature and workspace gml
    final FeatureType[] types = schema.getFeatureTypes();
    final Feature feature = FeatureFactory.createFeature( gml.getRootFeature(), types );

    return new GMLWorkspace_Impl( types, feature, context, schemaLocationName, schema.getTargetNS(),
        schema.getNamespaceMap() );
  }

  public static GMLWorkspace createGMLWorkspace( final InputStream inputStream, URL schemaURL )
      throws Exception
  {

    final GMLDocument_Impl gml = new GMLDocument_Impl( XMLHelper.getAsDOM( inputStream, true ) );

    final GMLFeature gmlFeature = gml.getRootFeature();

    final GMLSchema schema = GMLSchemaCache.getSchema( schemaURL );
    if( schema == null )
      throw new Exception( "Schema konnte nicht gefunden werden." );

    // create feature and workspace gml
    final FeatureType[] types = schema.getFeatureTypes();
    final Feature feature = FeatureFactory.createFeature( gmlFeature, types );

    return new GMLWorkspace_Impl( types, feature, null, null, schema.getTargetNS(), schema
        .getNamespaceMap() );
  }
}