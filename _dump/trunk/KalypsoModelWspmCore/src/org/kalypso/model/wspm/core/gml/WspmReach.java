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
package org.kalypso.model.wspm.core.gml;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

/**
 * @author belger
 */
public abstract class WspmReach implements IWspmConstants
{
  private final Feature m_reach;

  public WspmReach( final Feature reach )
  {
    // dont check, we are abstract; maybe check if qname substitutes?
    // if( !QNameUtilities.equals( reach.getFeatureType().getQName(), NS_WSPM_TUHH, "ReachWspmTuhhSteadyState" ) )
    // throw new IllegalStateException( "Feature is of wrong type: " + reach );

    m_reach = reach;
  }

  public String getName( )
  {
    return NamedFeatureHelper.getName( m_reach );
  }

  public void setName( final String name )
  {
    NamedFeatureHelper.setName( m_reach, name );
  }

  public String getDescription( )
  {
    return NamedFeatureHelper.getDescription( m_reach );
  }

  public void setDescription( final String desc )
  {
    NamedFeatureHelper.setDescription( m_reach, desc );
  }

  public void setWaterBody( final WspmWaterBody body )
  {
    final IPropertyType waterProp = m_reach.getFeatureType().getProperty( new QName( NS_WSPM, "waterBodyMember" ) );
    m_reach.setProperty( waterProp, body.getFeature().getId() );
  }

  public WspmWaterBody getWaterBody( )
  {
    final Object body = m_reach.getProperty( new QName( NS_WSPM, "waterBodyMember" ) );
    if( body instanceof Feature )
      return new WspmWaterBody( (Feature) body );
    else
    {
      final Feature feature = m_reach.getWorkspace().getFeature( (String) body );
      return new WspmWaterBody( feature );
    }
  }

  protected FeatureList getReachSegmentList( )
  {
    return (FeatureList) m_reach.getProperty( new QName( NS_WSPM, "reachSegmentMember" ) );
  }

  public Feature getFeature( )
  {
    return m_reach;
  }

}
