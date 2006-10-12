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

import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;
import javax.xml.transform.TransformerException;

import org.apache.xpath.XPathAPI;
import org.deegree.services.capabilities.DCPType;
import org.deegree.services.capabilities.HTTP;
import org.deegree.services.capabilities.Protocol;
import org.deegree.services.wfs.capabilities.WFSCapabilities;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author doemming
 */
public class WFSCapabilitiesVersion1 implements IWFSCapabilities
{

  private WFSCapabilities m_capabilities;

  private IWFSLayer[] m_layers = null;

  private Document m_dom;

  private String[] m_spatialOperators = new String[0];

  private String[] m_logicalOperators = new String[0];

  private String[] m_comparisionOperators = new String[0];

  private boolean m_simpleArithmeticOperators = false;

  private String[] m_functionArithmeticOperators = new String[0];

  public WFSCapabilitiesVersion1( final WFSCapabilities capabilities, final Document dom ) throws TransformerException
  {
    m_capabilities = capabilities;
    m_dom = dom;
    init();
  }

  private void init( ) throws TransformerException
  {
    final ArrayList<String> list = new ArrayList<String>();
    Node selectNode = XPathAPI.selectSingleNode( m_dom, "WFS_Capabilities/Filter_Capabilities/Spatial_Capabilities/Spatial_Operators/BBOX" );
    if( selectNode != null )
      list.add( "BBOX" );
    selectNode = XPathAPI.selectSingleNode( m_dom, "WFS_Capabilities/Filter_Capabilities/Spatial_Capabilities/Spatial_Operators/Equals" );
    if( selectNode != null )
      list.add( "Equals" );
    selectNode = XPathAPI.selectSingleNode( m_dom, "WFS_Capabilities/Filter_Capabilities/Spatial_Capabilities/Spatial_Operators/Disjoint" );
    if( selectNode != null )
      list.add( "Disjoint" );
    selectNode = XPathAPI.selectSingleNode( m_dom, "WFS_Capabilities/Filter_Capabilities/Spatial_Capabilities/Spatial_Operators/Intersect" );
    if( selectNode != null )
      list.add( "Intersect" );
    selectNode = XPathAPI.selectSingleNode( m_dom, "WFS_Capabilities/Filter_Capabilities/Spatial_Capabilities/Spatial_Operators/Touches" );
    if( selectNode != null )
      list.add( "Touches" );
    selectNode = XPathAPI.selectSingleNode( m_dom, "WFS_Capabilities/Filter_Capabilities/Spatial_Capabilities/Spatial_Operators/Crosses" );
    if( selectNode != null )
      list.add( "Crosses" );
    selectNode = XPathAPI.selectSingleNode( m_dom, "WFS_Capabilities/Filter_Capabilities/Spatial_Capabilities/Spatial_Operators/Within" );
    if( selectNode != null )
      list.add( "Within" );
    selectNode = XPathAPI.selectSingleNode( m_dom, "WFS_Capabilities/Filter_Capabilities/Spatial_Capabilities/Spatial_Operators/Contains" );
    if( selectNode != null )
      list.add( "Contains" );
    selectNode = XPathAPI.selectSingleNode( m_dom, "WFS_Capabilities/Filter_Capabilities/Spatial_Capabilities/Spatial_Operators/Overlaps" );
    if( selectNode != null )
      list.add( "Overlaps" );
    selectNode = XPathAPI.selectSingleNode( m_dom, "WFS_Capabilities/Filter_Capabilities/Spatial_Capabilities/Spatial_Operators/Beyond" );
    if( selectNode != null )
      list.add( "Beyond" );
    selectNode = XPathAPI.selectSingleNode( m_dom, "WFS_Capabilities/Filter_Capabilities/Spatial_Capabilities/Spatial_Operators/DWithin" );
    if( selectNode != null )
      list.add( "DWithin" );
    m_spatialOperators = list.toArray( new String[list.size()] );
    // Logical Ops
    final ArrayList<String> logicalOps = new ArrayList<String>();
    selectNode = XPathAPI.selectSingleNode( m_dom, "WFS_Capabilities/Filter_Capabilities/Scalar_Capabilities/Logical_Operators" );
    if( selectNode != null )
    {
      logicalOps.add( "And" );
      logicalOps.add( "Not" );
      logicalOps.add( "Or" );
      m_logicalOperators = logicalOps.toArray( new String[logicalOps.size()] );
    }
    // Comparsion Ops
    final ArrayList<String> comparisionOps = new ArrayList<String>();
    selectNode = XPathAPI.selectSingleNode( m_dom, "WFS_Capabilities/Filter_Capabilities/Scalar_Capabilities/Comparison_Operators/Simple_Comparisons" );
    if( selectNode != null )
    {
      comparisionOps.add( "PropertyIsEqualTo" );
      comparisionOps.add( "PropertyIsLessThan" );
      comparisionOps.add( "PropertyIsGreaterThan" );
      comparisionOps.add( "PropertyIsLessThanOrEqualTo" );
      comparisionOps.add( "PropertyIsGreaterThanOrEqualTo" );
    }
    selectNode = XPathAPI.selectSingleNode( m_dom, "WFS_Capabilities/Filter_Capabilities/Scalar_Capabilities/Comparison_Operators/Between" );
    if( selectNode != null )
      comparisionOps.add( "PropertyIsBetween" );
    selectNode = XPathAPI.selectSingleNode( m_dom, "WFS_Capabilities/Filter_Capabilities/Scalar_Capabilities/Comparison_Operators/Like" );
    if( selectNode != null )
      comparisionOps.add( "PropertyIsLike" );
    selectNode = XPathAPI.selectSingleNode( m_dom, "WFS_Capabilities/Filter_Capabilities/Scalar_Capabilities/Comparison_Operators/NullCheck" );
    if( selectNode != null )
      comparisionOps.add( "PropertyIsNull" );
    m_comparisionOperators = comparisionOps.toArray( new String[comparisionOps.size()] );

    // Simple arithemtics
    selectNode = XPathAPI.selectSingleNode( m_dom, "WFS_Capabilities/Filter_Capabilities/Scalar_Capabilities/Arithmetic_Operators/Simple_Arithmetic" );
    if( selectNode != null )
      m_simpleArithmeticOperators = true;

    // Function arithmetics
    if( selectNode != null )
      m_functionArithmeticOperators = getAsTexts( "WFS_Capabilities/Filter_Capabilities/Scalar_Capabilities/Arithmetic_Operators/Functions/Function_Names/Function_Name" );
  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getVersion()
   */
  public String getVersion( )
  {
    return m_capabilities.getVersion();
  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getBaseURLDescribeFeatureTypeRequest()
   */
  public URL[] getBaseURLDescribeFeatureTypeRequest( int methode )
  {
    final DCPType[] type = m_capabilities.getCapability().getRequest().getDescribeFeatureType().getDCPType();
    return getURLsFromDCPTypes( type, methode );
  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getBaseURLGetFeatureRequest()
   */
  public URL[] getBaseURLGetFeatureRequest( int methode )
  {
    final DCPType[] type = m_capabilities.getCapability().getRequest().getGetFeature().getDCPType();
    return getURLsFromDCPTypes( type, methode );
  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getBaseURLGetCapabilitiesRequest()
   */
  public URL[] getBaseURLGetCapabilitiesRequest( int methode )
  {
    final DCPType[] type = m_capabilities.getCapability().getRequest().getGetCapabilities().getDCPType();
    return getURLsFromDCPTypes( type, methode );
  }

  private URL[] getURLsFromDCPTypes( DCPType[] type, int methode )
  {
    for( int i = 0; i < type.length; i++ )
    {
      final DCPType dcpt = type[i];
      final Protocol protocol = dcpt.getProtocol();
      switch( methode )
      {
        case METHODE_HTTP_GET:
          if( protocol instanceof HTTP )
            return ((HTTP) protocol).getGetOnlineResources();
          break;
        case METHODE_HTTP_POST:
          if( protocol instanceof HTTP )
            return ((HTTP) protocol).getPostOnlineResources();
          break;
      }
    }
    return new URL[0];
  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getLayer()
   */
  public IWFSLayer[] getFeatureTypes( )
  {
    if( m_layers == null )
    {
      try
      {
        final QName[] ftQNames = getAsQNames( "WFS_Capabilities/FeatureTypeList/FeatureType/Name" );
        final String[] srs = getAsTexts( "WFS_Capabilities/FeatureTypeList/FeatureType/SRS" );
        m_layers = new IWFSLayer[ftQNames.length];
        for( int i = 0; i < ftQNames.length; i++ )
        {
          URL url = WFSUtilities.createDescribeFeatureTypeRequestURL( this, ftQNames[i] );
          m_layers[i] = new WFSLayer( ftQNames[i], ftQNames[i].getLocalPart(), url, srs[i] );
        }
      }
      catch( Exception e )
      {
        e.printStackTrace();
        // do nothing
      }

    }

    return m_layers;

  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getGetFeatureOutputFormats()
   */
  public String[] getGetFeatureOutputFormats( )
  {
    return m_capabilities.getCapability().getRequest().getGetFeature().getResultFormat();
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
        String namespace = node.lookupNamespaceURI( parts[0] );
        if( namespace == null )
          namespace = m_dom.lookupNamespaceURI( parts[0] );
        final String localName = parts[1];
        result.add( new QName( namespace, localName ) );
      }
      else
        result.add( new QName( textContent ) );
    }
    return result.toArray( new QName[result.size()] );
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
    return m_comparisionOperators;
  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getSimpleArithmetics()
   */
  public boolean canDoSimpleArithmetics( )
  {
    return m_simpleArithmeticOperators;
  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getFunctionArithmetics()
   */
  public String[] getFunctionArithmetics( )
  {
    return m_functionArithmeticOperators;
  }

  /**
   * @see org.kalypso.ogc.wfs.IWFSCapabilities#getGeometryOperands()
   */
  public String[] getGeometryOperands( )
  {
    return new String[0];
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

}
