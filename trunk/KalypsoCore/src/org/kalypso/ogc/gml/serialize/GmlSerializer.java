package org.kalypso.ogc.gml.serialize;

import java.io.InputStream;
import java.io.InputStreamReader;
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

import org.apache.tools.ant.filters.ReplaceTokens;
import org.apache.tools.ant.filters.ReplaceTokens.Token;
import org.deegree.gml.GMLDocument;
import org.deegree.gml.GMLFeature;
import org.deegree.gml.GMLNameSpace;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.gml.GMLDocument_Impl;
import org.deegree_impl.gml.GMLFactory;
import org.deegree_impl.gml.GMLNameSpace_Impl;
import org.deegree_impl.gml.schema.GMLSchema;
import org.deegree_impl.gml.schema.XMLHelper;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.feature.GMLWorkspace_Impl;
import org.kalypso.java.net.IUrlResolver;
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
    try
    {
      final GMLDocument gmlDoc = new GMLDocument_Impl();
      
      final String schemaNamespace = workspace.getSchemaNamespace();
      if( schemaNamespace != null )
      {
        final GMLNameSpace namespace = new GMLNameSpace_Impl(null, schemaNamespace );
        gmlDoc.addNameSpace( namespace );
      }
      
      final GMLNameSpace gmlNameSpace = new GMLNameSpace_Impl(
          "gml","http://www.opengis.net/gml" );
      final GMLNameSpace xlinkNameSpace = new GMLNameSpace_Impl(
          "xlink","http://www.w3.org/1999/xlink" );
      final GMLNameSpace xsiNameSpace = new GMLNameSpace_Impl(
          "xsi","http://www.w3.org/2001/XMLSchema-instance" );
      gmlDoc.addNameSpace( gmlNameSpace );
      gmlDoc.addNameSpace( xlinkNameSpace );
      gmlDoc.addNameSpace( xsiNameSpace );

      GMLFeature gmlFeature = GMLFactory.createGMLFeature( gmlDoc, workspace
          .getRootFeature() );
      gmlDoc.setRoot( gmlFeature );

      
      workspace.getContext();
      final String schemaLoc = workspace.getSchemaLocation();
      if( schemaLoc != null )
        gmlDoc.setSchemaLocation( schemaLoc );

      // DOM als GML schreiben
      final Document xmlDOM = gmlDoc;
      final Transformer t = TransformerFactory.newInstance().newTransformer();
      
      t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" );
      t.setOutputProperty( OutputKeys.INDENT, "yes" );

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
    final GMLDocument_Impl gml = new GMLDocument_Impl( XMLHelper.getAsDOM( gmlURL ) );

    final GMLFeature gmlFeature = gml.getRootFeature();

    // load schema
    final GMLSchema schema = new GMLSchema( schemaURL );

    // create feature and workspace gml
    final FeatureType[] types = schema.getFeatureTypes();
    final Feature feature = FeatureFactory.createFeature( gmlFeature, types );

    // nicht die echte URL der schemaLocation, sondern dass, was im gml steht!
    final String schemaLocationName = gml.getSchemaLocationName();
    
    return new GMLWorkspace_Impl( types, feature, gmlURL, schemaLocationName, schema.getTargetNS() );
  }

  public static GMLWorkspace createGMLWorkspace( final URL gmlURL, final IUrlResolver urlResolver )
      throws Exception
  {
    // Replace tokens
    final URLConnection connection = gmlURL.openConnection();
    String contentEncoding = connection.getContentEncoding();
    if( contentEncoding == null )
      contentEncoding = "UTF-8";

    final InputStream inputStream = connection.getInputStream();
    final InputStreamReader isr = new InputStreamReader( inputStream, contentEncoding );

    final ReplaceTokens rt = new ReplaceTokens( isr );
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
    final Document gmlAsDOM = XMLHelper.getAsDOM( inputSource );

    final GMLDocument_Impl gml = new GMLDocument_Impl( gmlAsDOM );

    // load schema
    final String schemaLocationName = gml.getSchemaLocationName();
    if( schemaLocationName == null || schemaLocationName.length() == 0 )
      throw new Exception( "Keine 'schemaLocation' in gml spezifiziert." );

    final URL schemaLocation = urlResolver.resolveURL( gmlURL, schemaLocationName );
    final GMLSchema schema = new GMLSchema( schemaLocation );

    // create feature and workspace gml
    final FeatureType[] types = schema.getFeatureTypes();
    final Feature feature = FeatureFactory.createFeature( gml.getRootFeature(), types );

    return new GMLWorkspace_Impl( types, feature, gmlURL, schemaLocationName, schema.getTargetNS() );
  }
}