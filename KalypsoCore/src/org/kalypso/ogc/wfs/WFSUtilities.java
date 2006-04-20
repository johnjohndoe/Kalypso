/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.wfs;

import java.io.BufferedInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.deegree.services.wfs.capabilities.WFSCapabilities;
import org.deegree_impl.services.wfs.capabilities.WFSCapabilitiesFactory;
import org.kalypso.contribs.java.lang.MultiException;
import org.kalypso.contribs.java.xml.XMLHelper;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.visitors.ResortVisitor;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;

/**
 * @author doemming
 */
public class WFSUtilities
{

  public static IWFSCapabilities getCapabilites( final URL baseURL ) throws Exception
  {
    final MultiException multiExcepts = new MultiException();
    // TODO: we get problems here, if we already have a query part in the url
    final URL urlGetCap = new URL( baseURL + "?" + "SERVICE=WFS&REQUEST=GetCapabilities" );
    try
    {
      // try deegree1 implementation
      final URLConnection conGetCap = urlGetCap.openConnection();
      final InputStream isGetCap = conGetCap.getInputStream();
      final Reader reader = new InputStreamReader( isGetCap );
      final WFSCapabilities capabilities = WFSCapabilitiesFactory.createCapabilities( reader );
      return new WFSCapabilitiesDeegree1( capabilities );
    }
    catch( Exception e )
    {
      // e.printStackTrace();
      multiExcepts.addException( e );
    }
    try
    {
      // hack
      // File file = new File( "D:/eclipse3.1/tmp/XPlanungCaps.xml" );
      // URL url = file.toURL();
      final Document dom = XMLHelper.getAsDOM( urlGetCap, true );
      return new WFSCapabilitiesFromDOM( dom );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      multiExcepts.addException( e );
    }
    throw multiExcepts;
  }

  public static String buildGetFeatureRequestPOST( final IWFSCapabilities wfsCaps, final QName ftQName, final String filter, final String maxFeatureAsString )
  {
    final StringBuffer sb = new StringBuffer();
    // final int maxFeatures = 5000;

    sb.append( "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>\n" );// iso-8859-1
    sb.append( "<wfs:GetFeature " );
    final String[] outFormats = wfsCaps.getGetFeatureOutputFormats();
    String oFormat = null;
    if( outFormats.length > 0 )
    {
      for( int i = 0; i < outFormats.length; i++ )
      {
        if( outFormats[i].equalsIgnoreCase( "gml2" ) )
        {
          oFormat = outFormats[i];
          break;
        }
      }
      if( oFormat == null )
        oFormat = outFormats[0];
    }
    if( oFormat != null )
      sb.append( " outputFormat=\"" + oFormat + "\"" );
    // sb.append( " outputFormat=\"GML2\"" );

    String version = wfsCaps.getVersion();
    if( version != null && version.length() > 0 )
      sb.append( " version=\"" + version + "\" " );
    sb.append( " xmlns:gml=\"http://www.opengis.net/gml\" " );
    sb.append( " xmlns:wfs=\"http://www.opengis.net/wfs\"" );
    sb.append( " xmlns:ogc=\"http://www.opengis.net/ogc\"" );
    if( maxFeatureAsString != null && maxFeatureAsString.length() > 0 )
      sb.append( " maxFeatures=\"" + maxFeatureAsString + "\"" );
    sb.append( " >\n" );
    String namespaceURI = ftQName.getNamespaceURI();
    String localPart = ftQName.getLocalPart();
    if( version == null ) // deegree1 gazetteer
      sb.append( "<wfs:Query typeName=\"" + localPart + "\">\n" );
    else if( namespaceURI != null && namespaceURI.length() > 0 )
    {
      sb.append( "<wfs:Query typeName=\"sn99:" + localPart + "\" xmlns:sn99=\"" + namespaceURI + "\">\n" );
    }
    else
      sb.append( "<wfs:Query typeName=\"" + localPart + "\">\n" );
    if( filter != null && filter.length() > 0 )
    {
      sb.append( filter ).append( "\n" );
      // sb.append( "<ogc:Filter>\n" );
      // sb.append( "<ogc:PropertyIsEqualTo wildCard=\"*\" singleChar=\"#\" escape=\"!\">\n" );
      // sb.append( "<ogc:PropertyName>KENNZAHL</ogc:PropertyName>\n" );
      // sb.append( "<ogc:Literal>59519000</ogc:Literal>\n" );
      // sb.append( "</ogc:PropertyIsEqualTo>\n" );
      // sb.append( "</ogc:Filter>\n" );
    }
    sb.append( "</wfs:Query>\n" );
    sb.append( "</wfs:GetFeature>" );
    return sb.toString();
  }

  /**
   * @param baseURL
   * @param featureTypeToLoad
   * @param targetCRS
   *          optional coordinate system
   * @param filter
   *          optional filter
   * @return gmlworkspace
   * @throws Exception
   */
  public static GMLWorkspace createGMLWorkspaceFromGetFeature( final URL baseURL, final QName featureTypeToLoad, final CS_CoordinateSystem targetCRS, final String filter, final String maxFeatureAsString ) throws Exception
  {
    BufferedInputStream inputStream = null;
    PrintStream postWriter = null;
    try
    {
      final IWFSCapabilities wfsCaps = WFSUtilities.getCapabilites( baseURL );

      final URL[] baseURLGetCapabilitiesRequest = wfsCaps.getBaseURLGetFeatureRequest( IWFSCapabilities.METHODE_HTTP_POST );
      if( baseURLGetCapabilitiesRequest.length > 0 )
      {

        final URLConnection con = baseURLGetCapabilitiesRequest[0].openConnection();
        con.setDoOutput( true );
        con.setDoInput( true );

        // write request to the WFS server
        OutputStream connectionOutputStream = con.getOutputStream();
        postWriter = new PrintStream( connectionOutputStream );

        final String getFeaturePost = WFSUtilities.buildGetFeatureRequestPOST( wfsCaps, featureTypeToLoad, filter, maxFeatureAsString );
        postWriter.print( getFeaturePost );

        // read response from the WFS server and create a GMLWorkspace
        inputStream = new BufferedInputStream( con.getInputStream() );

        // Hack for testing
        // final File tmpFile = new File( "D:/eclipse3.1/tmp/gml.gml" );
        // final File tmpFile = new File( "C:/TMP/gml.gml" );
        // final OutputStream outStream = new FileOutputStream( tmpFile );
        // StreamUtilities.streamCopy( inputStream, outStream );
        // IOUtils.closeQuietly( inputStream );
        // IOUtils.closeQuietly( outStream );

        final URL schemaURLHint = createDescribeFeatureTypeRequestURL( wfsCaps, featureTypeToLoad );
        // final GMLSchema gmlSchema = GMLSchemaFactory.createGMLSchema(schemaURL);

        final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( inputStream, schemaURLHint, false );
        inputStream.close();
        IOUtils.closeQuietly( inputStream );

        if( targetCRS != null )
          workspace.accept( new TransformVisitor( targetCRS ), workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
        workspace.accept( new ResortVisitor(), workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );

        return workspace;
      }
      throw new UnsupportedOperationException( "GetFeature-Request: HTTP/Get is not supportet" );
    }
    finally
    {
      IOUtils.closeQuietly( postWriter );
      IOUtils.closeQuietly( inputStream );
    }
  }

  /**
   * @param baseURL
   * @return url to describefeaturetyperequest
   * @throws MalformedURLException
   */
  public static URL createDescribeFeatureTypeRequestURL( IWFSCapabilities wfsCaps, final QName ftQName ) throws MalformedURLException
  {
    final URL[] baseURL = wfsCaps.getBaseURLDescribeFeatureTypeRequest( IWFSCapabilities.METHODE_HTTP_GET );
    final String baseURLAsString = baseURL[0].toString();
    if( wfsCaps.getVersion() == null || wfsCaps.getVersion().startsWith( "1.0" ) )
    {
      // TODO support namespaces
      if( baseURL.length > 0 )
      {
        final StringBuffer result = new StringBuffer( baseURLAsString );
        if( baseURLAsString.indexOf( "?" ) < 0 )
          result.append( "?" );
        result.append( "SERVICE=WFS&VERSION=1.0.0&REQUEST=DescribeFeatureType" );
        result.append( "&typename=" + ftQName.getLocalPart() );
        return new URL( result.toString() );
      }
    }
    if( wfsCaps.getVersion().startsWith( "1.1" ) )
    {
      if( baseURL.length > 0 )
      {
        final StringBuffer result = new StringBuffer( baseURLAsString );
        if( baseURLAsString.indexOf( "?" ) < 0 )
          result.append( "?" );
        result.append( "SERVICE=WFS&VERSION=1.1.0&REQUEST=DescribeFeatureType" );
        final String localPart = ftQName.getLocalPart();
        final String namespaceURI = ftQName.getNamespaceURI();
        final String prefix = ftQName.getPrefix();
        // check if featuretype is qualified
        if( namespaceURI == null || namespaceURI.length() < 1 )
          result.append( "&TYPENAME=" + ftQName.getLocalPart() );
        // check if prefix is provided
        else if( prefix != null && prefix.length() > 0 )
        {
          result.append( "&TYPENAME=" + prefix + ":" + localPart );
          result.append( "&NAMESPACE=xmlns(" + prefix + "=" + namespaceURI + ")" );
        }
        // use standard prefix
        else
        {
          result.append( "&TYPENAME=" + "sn99:" + localPart );
          result.append( "&NAMESPACE=xmlns(sn99=" + namespaceURI + ")" );
        }
        // TODO check for chars to escape in namespace
        return new URL( result.toString() );
//      http://wfs.lat-lon.de/deegreegazetteer/ogcwebservice?REQUEST=DescribeFeatureType&version=1.1.0&service=WFS&TYPENAME=ns:gns_locationinstance&NAMESPACE=xmlns(ns=http://www.deegree.org/app)  
//     http://wfs.lat-lon.de/deegreegazetteer/ogcwebservice?REQUEST=GetCapabilities&version=1.1.0&service=WFS   
      }
    }
    throw new UnsupportedOperationException();
  }
}
