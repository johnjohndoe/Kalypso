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

import org.deegree.services.capabilities.DCPType;
import org.deegree.services.capabilities.HTTP;
import org.deegree.services.capabilities.Protocol;
import org.deegree.services.wfs.capabilities.FeatureType;
import org.deegree.services.wfs.capabilities.FeatureTypeList;
import org.deegree.services.wfs.capabilities.WFSCapabilities;

/**
 * @author FlowsAd
 */
public class WFSCapabilitiesDeegree1 implements IWFSCapabilities
{

  private final WFSCapabilities m_capabilities;

  private IWFSLayer[] m_layers = null;

  public WFSCapabilitiesDeegree1( WFSCapabilities capabilities )
  {
    m_capabilities = capabilities;
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
      List<IWFSLayer> list = new ArrayList<IWFSLayer>();
      final FeatureTypeList featureTypeList = m_capabilities.getFeatureTypeList();
      final FeatureType[] featureTypes = featureTypeList.getFeatureTypes();
      for( int i = 0; i < featureTypes.length; i++ )
      {
        final FeatureType ft = featureTypes[i];
        final String name = ft.getName();
        final String title = ft.getTitle();
        QName qName = new QName( name );
        list.add( new WFSLayer( qName, title ) );
      }
      m_layers = list.toArray( new IWFSLayer[list.size()] );
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
}
