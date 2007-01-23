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

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.contribs.javax.xml.namespace.QNameUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.binding.NamedFeatureHelper;

/**
 * @author Gernot Belger
 */
public class WspmWaterBody implements IWspmConstants
{
  public final static QName QNAME_WSP_FIX_MEMBER = new QName( NS_WSPM, "waterlevelFixationMember" );

  public static final QName QNAME_REACH_MEMBER = new QName( NS_WSPM, "reachMember" );

  public static final QName QNAME_PROP_PROFILEMEMBER = new QName( NS_WSPM, "profileMember" );

  private final Feature m_water;


  public WspmWaterBody( final Feature water )
  {
    if( !QNameUtilities.equals( water.getFeatureType().getQName(), IWspmConstants.NS_WSPM, "WaterBody" ) )
      throw new IllegalStateException( "Feature is of wrong type: " + water );

    m_water = water;
  }

  public String getName( )
  {
    return NamedFeatureHelper.getName( getFeature() );
  }

  public void setName( final String name )
  {
    NamedFeatureHelper.setName( getFeature(), name );
  }

  public String getDescription( )
  {
    return NamedFeatureHelper.getDescription( getFeature() );
  }

  public void setDescription( final String desc )
  {
    NamedFeatureHelper.setDescription( getFeature(), desc );
  }

  public Feature getFeature( )
  {
    return m_water;
  }

  public WspmProfile createNewProfile( ) throws GMLSchemaException
  {
    final Feature profile = FeatureHelper.addFeature( m_water, QNAME_PROP_PROFILEMEMBER, new QName( NS_WSPMPROF, "Profile" ) );

    return new WspmProfile( profile );
  }

  public void setRefNr( final String refNr )
  {
    m_water.setProperty( new QName( NS_WSPM, "refNr" ), refNr );
  }

  public void setDirectionUpstreams( final boolean directionIsUpstream )
  {
    m_water.setProperty( new QName( NS_WSPM, "isDirectionUpstream" ), new Boolean( directionIsUpstream ) );
  }

  public Feature createRunOffEvent( ) throws GMLSchemaException
  {
    return FeatureHelper.addFeature( m_water, new QName( NS_WSPM, "runOffEventMember" ), new QName( NS_WSPMRUNOFF, "RunOffEvent" ) );
  }

  public Feature createWspFix( ) throws GMLSchemaException
  {
    return FeatureHelper.addFeature( m_water, QNAME_WSP_FIX_MEMBER, new QName( NS_WSPMRUNOFF, "WaterlevelFixation" ) );
  }

  public List getWspFixations( )
  {
    return (List) m_water.getProperty( QNAME_WSP_FIX_MEMBER );
  }

  public boolean isDirectionUpstreams( )
  {
    return (Boolean) m_water.getProperty( new QName( NS_WSPM, "isDirectionUpstream" ) );
  }

  public WspmReach[] getReaches( )
  {
    final FeatureList reaches = (FeatureList) getFeature().getProperty( QNAME_REACH_MEMBER );
    final List<WspmReach> reachList = new ArrayList<WspmReach>( reaches.size() );
    for( final Object object : reaches )
    {
      final Feature f = (Feature) object;
      reachList.add( new WspmReach( f ){} );
    }

    return reachList.toArray( new WspmReach[reachList.size()] );
  }
}
