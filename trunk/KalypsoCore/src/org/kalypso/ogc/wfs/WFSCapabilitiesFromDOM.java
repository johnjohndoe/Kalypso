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
package org.kalypso.ogc.wfs;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;
import javax.xml.transform.TransformerException;

import org.apache.xpath.XPathAPI;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author doemming
 */
public class WFSCapabilitiesFromDOM implements IWFSCapabilities
{

  private final Document m_dom;

  private String m_version;

  private URL[] m_baseGetCapabilitiesRequestGET;

  private URL[] m_baseGetCapabilitiesRequestPOST;

  private URL[] m_baseDescribeFeatureTypeRequestGET;

  private URL[] m_baseDescribeFeatureTypeRequestPOST;

  private URL[] m_baseGetFeatureRequestGET;

  private URL[] m_baseGetFeatureRequestPOST;

  private IWFSLayer[] m_ftLayers = null;

  private String[] m_getFeatureOutputFormats;

  private String[] m_spatialOperators;

  private String[] m_logicalOperators;

  private String[] m_comparisonOperators;

  private String[] m_geometryOperands;

  private boolean m_simpleArithmetics = false;

  private String[] m_functionArithmetics;

  public WFSCapabilitiesFromDOM( Document dom )
  {
    m_dom = dom;
    init();
  }

  private void init( )
  {
    try
    {
      // init version
      m_version = getAsText( "wfs:WFS_Capabilities/@version" );
      // String[] m_filter = getAsTexts( "ogc:Filter_Capabilities/ogc:Spatial_Capabilities/ogc:Spatial_Operators");
      // init urls
      m_baseGetCapabilitiesRequestGET = getURLS( "GetCapabilities", METHODE_HTTP_GET );
      m_baseGetCapabilitiesRequestPOST = getURLS( "GetCapabilities", METHODE_HTTP_POST );
      m_baseDescribeFeatureTypeRequestGET = getURLS( "DescribeFeatureType", METHODE_HTTP_GET );
      m_baseDescribeFeatureTypeRequestPOST = getURLS( "DescribeFeatureType", METHODE_HTTP_POST );
      m_baseGetFeatureRequestGET = getURLS( "GetFeature", METHODE_HTTP_GET );
      m_baseGetFeatureRequestPOST = getURLS( "GetFeature", METHODE_HTTP_POST );

      m_getFeatureOutputFormats = getAsTexts( "wfs:WFS_Capabilities/ows:OperationsMetadata/ows:Operation[@name='GetFeature']/ows:Parameter[@name='outputFormat']/ows:Value" );
      // init layers

      final QName[] ftQNames = getAsQNames( "wfs:WFS_Capabilities/wfs:FeatureTypeList/wfs:FeatureType/wfs:Name" );
      final String[] srs = getAsTexts( "wfs:WFS_Capabilities/wfs:FeatureTypeList/wfs:FeatureType/wfs:DefaultSRS" );
      m_ftLayers = new IWFSLayer[ftQNames.length];
      for( int i = 0; i < ftQNames.length; i++ )
      {
        URL url = WFSUtilities.createDescribeFeatureTypeRequestURL( this, ftQNames[i] );
        m_ftLayers[i] = new WFSLayer( ftQNames[i], ftQNames[i].getLocalPart(), url, srs[i] );
      }
      m_geometryOperands = getAsTexts( "wfs:WFS_Capabilities/ogc:Filter_Capabilities/ogc:Spatial_Capabilities/ogc:GeometryOperands/ogc:GeometryOperand" );
      m_spatialOperators = getAsTexts( "wfs:WFS_Capabilities/ogc:Filter_Capabilities/ogc:Spatial_Capabilities/ogc:SpatialOperators/ogc:SpatialOperator" );
      m_logicalOperators = getAsTexts( "wfs:WFS_Capabilities/ogc:Filter_Capabilities/ogc:Scalar_Capabilities/ogc:LogicalOperators/ogc:LogicalOperator" );
      m_comparisonOperators = getAsTexts( "wfs:WFS_Capabilities/ogc:Filter_Capabilities/ogc:Scalar_Capabilities/ogc:ComparisonOperators/ogc:ComparisonOperator" );
      if( getAsText( "wfs:WFS_Capabilities/ogc:Filter_Capabilities/ogc:Scalar_Capabilities/ogc:ArithmeticOperators/ogc:SimpleArithmetic" ) != null )
        m_simpleArithmetics = true;
      m_functionArithmetics = getAsTexts( "wfs:WFS_Capabilities/ogc:Filter_Capabilities/ogc:Scalar_Capabilities/ogc:ArithmeticOperators/ogc:Functions/ogc:FunctionNames" );

    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

  }

  private QName[] getAsQNames( String xPath ) throws TransformerException
  {
    final List<QName> result = new ArrayList<QName>();
    final NodeList list = XPathAPI.selectNodeList( m_dom, xPath );
    int length = list.getLength();

    for( int i = 0; i < length; i++ )
    {
      final Node node = list.item( i );
      final String textContent = node.getTextContent();
      if( textContent.indexOf( ":" ) > 0 )
      {
        final String[] parts = textContent.split( ":" );
        // prefix:
        final String namespace = node.lookupNamespaceURI( parts[0] );
        final String localName = parts[1];
        result.add( new QName( namespace, localName ) );
      }
      else
        result.add( new QName( textContent ) );
    }
    return result.toArray( new QName[result.size()] );
  }

  private URL[] getURLS( String request, int methode ) throws TransformerException
  {
    final List<URL> result = new ArrayList<URL>();
    final String methodeString;
    switch( methode )
    {
      case METHODE_HTTP_POST:
        methodeString = "Post";
        break;

      case METHODE_HTTP_GET:
      default:
        methodeString = "Get";
        break;
    }
    final String urlAsStrings[] = getAsTexts( "wfs:WFS_Capabilities/ows:OperationsMetadata/ows:Operation[@name='" + request + "']/ows:DCP/ows:HTTP/ows:" + methodeString + "/@xlink:href" );
    for( int i = 0; i < urlAsStrings.length; i++ )
    {
      try
      {
        final URL url = new URL( urlAsStrings[i] );
        result.add( url );
      }
      catch( MalformedURLException e )
      {
        e.printStackTrace();
      }
    }
    return result.toArray( new URL[result.size()] );
  }

  private String getAsText( String xPath ) throws DOMException, TransformerException
  {
    return XPathAPI.selectSingleNode( m_dom, xPath ).getTextContent();
  }

  private String[] getAsTexts( String xPath ) throws TransformerException
  {
    final List<String> result = new ArrayList<String>();
    final NodeList list = XPathAPI.selectNodeList( m_dom, xPath );
    int length = list.getLength();

    for( int i = 0; i < length; i++ )
    {
      final Node node = list.item( i );
      final String textContent = node.getTextContent();
      result.add( textContent );
    }
    return result.toArray( new String[result.size()] );
  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getVersion()
   */
  public String getVersion( )
  {
    return m_version;
  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getBaseURLDescribeFeatureTypeRequest(int)
   */
  public URL[] getBaseURLDescribeFeatureTypeRequest( int methode )
  {
    switch( methode )
    {
      case METHODE_HTTP_POST:
        return m_baseDescribeFeatureTypeRequestPOST;
      case METHODE_HTTP_GET:
      default:
        return m_baseDescribeFeatureTypeRequestGET;
    }
  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getBaseURLGetFeatureRequest(int)
   */
  public URL[] getBaseURLGetFeatureRequest( int methode )
  {
    switch( methode )
    {
      case METHODE_HTTP_POST:
        return m_baseGetFeatureRequestPOST;
      case METHODE_HTTP_GET:
      default:
        return m_baseGetFeatureRequestGET;
    }
  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getBaseURLGetCapabilitiesRequest(int)
   */
  public URL[] getBaseURLGetCapabilitiesRequest( int methode )
  {
    switch( methode )
    {
      case METHODE_HTTP_POST:
        return m_baseGetCapabilitiesRequestPOST;
      case METHODE_HTTP_GET:
      default:
        return m_baseGetCapabilitiesRequestGET;
    }
  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getFeatureTypes()
   */
  public IWFSLayer[] getFeatureTypes( )
  {
    return m_ftLayers;
  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getGetFeatureOutputFormats()
   */
  public String[] getGetFeatureOutputFormats( )
  {
    return m_getFeatureOutputFormats;
  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getSpatialOperators()
   */
  public String[] getSpatialOperators( )
  {
    return m_spatialOperators;
  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getLogicalOperators()
   */
  public String[] getLogicalOperators( )
  {
    return m_logicalOperators;
  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getComparisonOperators()
   */
  public String[] getComparisonOperators( )
  {
    return m_comparisonOperators;
  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getGeometryOperands()
   */
  public String[] getGeometryOperands( )
  {
    return m_geometryOperands;
  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getSimpleArithmetics()
   */
  public boolean canDoSimpleArithmetics( )
  {
    return m_simpleArithmetics;
  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getFunctionArithmetics()
   */
  public String[] getFunctionArithmetics( )
  {
    return m_functionArithmetics;
  }

}
