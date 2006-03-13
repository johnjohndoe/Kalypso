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
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;

import org.apache.commons.io.IOUtils;
import org.deegree.services.capabilities.DCPType;
import org.deegree.services.capabilities.HTTP;
import org.deegree.services.capabilities.Protocol;
import org.deegree.services.wfs.capabilities.Capability;
import org.deegree.services.wfs.capabilities.GetFeature;
import org.deegree.services.wfs.capabilities.Request;
import org.deegree.services.wfs.capabilities.WFSCapabilities;
import org.deegree_impl.services.wfs.capabilities.WFSCapabilitiesFactory;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.visitors.ResortVisitor;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.opengis.cs.CS_CoordinateSystem;
import org.xml.sax.SAXException;

/**
 * 
 * @author doemming
 */
public class WFSUtilities
{

  public static WFSCapabilities getCapabilites( final URL baseURL ) throws IOException, SAXException, Exception
  {
    final URL urlGetCap = new URL( baseURL + "?" + "SERVICE=WFS&VERSION=1.0.0&REQUEST=GetCapabilities" );
    final URLConnection conGetCap = urlGetCap.openConnection();
    final InputStream isGetCap = conGetCap.getInputStream();
    final Reader reader = new InputStreamReader( isGetCap );
    return WFSCapabilitiesFactory.createCapabilities( reader );
  }

  public static String buildGetFeatureRequestPOST( final String featureTypeToLoad, final String filter,
      final String maxFeatureAsString )
  {
    final StringBuffer sb = new StringBuffer();
    //    final int maxFeatures = 5000;

    sb.append( "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>\n" );// iso-8859-1
    sb.append( "<wfs:GetFeature outputFormat=\"GML2\"" );
    sb.append( " xmlns:gml=\"http://www.opengis.net/gml\" " );
    sb.append( " xmlns:wfs=\"http://www.opengis.net/wfs\"" );
    sb.append( " xmlns:ogc=\"http://www.opengis.net/ogc\"" );
    if( maxFeatureAsString != null && maxFeatureAsString.length() > 0 )
      sb.append( " maxFeatures=\"" + maxFeatureAsString + "\"" );
    sb.append( " >\n" );
    sb.append( "<wfs:Query typeName=\"" + featureTypeToLoad + "\">\n" );
    if( filter != null && filter.length() > 0 )
    {
      sb.append( filter ).append( "\n" );
      //      sb.append( "<ogc:Filter>\n" );
      //      sb.append( "<ogc:PropertyIsEqualTo wildCard=\"*\" singleChar=\"#\" escape=\"!\">\n" );
      //      sb.append( "<ogc:PropertyName>VERSKLASSE</ogc:PropertyName>\n" );
      //      sb.append( "<ogc:Literal>3</ogc:Literal>\n" );
      //      sb.append( "</ogc:PropertyIsEqualTo>\n" );
      //      sb.append( "</ogc:Filter>\n" );
    }
    sb.append( "</wfs:Query>\n" );
    sb.append( "</wfs:GetFeature>" );
    return sb.toString();
  }

  /**
   * 
   * @param baseURL
   * @param featureTypeToLoad
   * @param targetCRS
   *          optional coordinate system
   * @param filter
   *          optional filter
   * @return gmlworkspace
   * @throws Exception
   */
  public static GMLWorkspace createGMLWorkspaceFromGetFeature( final URL baseURL, final String featureTypeToLoad,
      final CS_CoordinateSystem targetCRS, final String filter, final String maxFeatureAsString ) throws Exception
  {
    BufferedInputStream inputStream = null;
    PrintStream postWriter = null;

    try
    {
      //      URL[] getOnlineResources = null;
      URL[] postOnlineResources = null;

      //      URL m_schemaURL = new URL( baseURL + "?SERVICE=WFS&VERSION=1.0.0&REQUEST=DescribeFeatureType&typeName="
      //          + featureTypeToLoad );
      //       get wfs capabiliets to check which protocol types are supported by the service
      final WFSCapabilities wfsCaps = WFSUtilities.getCapabilites( baseURL );

      final Capability capability = wfsCaps.getCapability();
      final Request request = capability.getRequest();
      final GetFeature getFeature = request.getGetFeature();
      final DCPType[] type = getFeature.getDCPType();
      for( int i = 0; i < type.length; i++ )
      {
        final DCPType dcpt = type[i];
        final Protocol protocol = dcpt.getProtocol();
        if( protocol instanceof HTTP )
        {
          //          getOnlineResources = ( (HTTP)protocol ).getGetOnlineResources();
          postOnlineResources = ( (HTTP)protocol ).getPostOnlineResources();
          //          if( getOnlineResources.length > 0 )
          //            getProtocol = true;
          //          if( postOnlineResources.length > 0 )
          //            postProtocol = true;
        }
      }
      if( postOnlineResources.length > 0 )
      {
        final URLConnection con = postOnlineResources[0].openConnection();
        con.setDoOutput( true );
        con.setDoInput( true );

        // write request to the WFS server
        OutputStream connectionOutputStream = con.getOutputStream();
        postWriter = new PrintStream( connectionOutputStream );

        final String getFeaturePost = WFSUtilities.buildGetFeatureRequestPOST( featureTypeToLoad, filter,
            maxFeatureAsString );
        postWriter.print( getFeaturePost );

        //read response from the WFS server and create a GMLWorkspace
        inputStream = new BufferedInputStream( con.getInputStream() );

        // Hack for testing
        //                final File tmpFile = new File( "C:\\TMP\\test.txt" );
        //                final OutputStream outStream = new FileOutputStream( tmpFile );
        //                StreamUtilities.streamCopy( inputStream, outStream );
        //                IOUtils.closeQuietly( inputStream );
        //                IOUtils.closeQuietly( outStream );

        final URL schemaURL = createDescribeFeatureTypeRequestURL( baseURL, featureTypeToLoad );
        final GMLSchema gmlSchema = GMLSchemaFactory.createGMLSchema(schemaURL);
        final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( inputStream, gmlSchema );
        inputStream.close();
        IOUtils.closeQuietly( inputStream );

        if( targetCRS != null )
          workspace.accept( new TransformVisitor( targetCRS ), workspace.getRootFeature(),
              FeatureVisitor.DEPTH_INFINITE );
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
  private static URL createDescribeFeatureTypeRequestURL( final URL baseURL, final String featureType )
      throws MalformedURLException
  {
    final StringBuffer result = new StringBuffer( baseURL.toString() );
    result.append( "?SERVICE=WFS&VERSION=1.0.0&REQUEST=DescribeFeatureType" );
    result.append( "&typename=" + featureType );
    return new URL( result.toString() );
  }
}
