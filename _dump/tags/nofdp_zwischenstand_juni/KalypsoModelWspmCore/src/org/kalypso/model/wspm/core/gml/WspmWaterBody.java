/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public class WspmWaterBody extends AbstractFeatureBinder implements IWspmConstants
{
  public final static QName QNAME = new QName( NS_WSPM, "WaterBody" ); //$NON-NLS-1$

  public final static QName QNAME_WSP_FIX_MEMBER = new QName( NS_WSPM, "waterlevelFixationMember" ); //$NON-NLS-1$

  public static final QName QNAME_REACH_MEMBER = new QName( NS_WSPM, "reachMember" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_PROFILEMEMBER = new QName( NS_WSPM, "profileMember" ); //$NON-NLS-1$

  public WspmWaterBody( final Feature water )
  {
    super( water, QNAME );
  }

  public WspmProfile createNewProfile( ) throws GMLSchemaException
  {
    final Feature profile = FeatureHelper.addFeature( getFeature(), QNAME_PROP_PROFILEMEMBER, WspmProfile.QNAME_PROFILE );

    return new WspmProfile( profile );
  }

  public void setRefNr( final String refNr )
  {
    setProperty( new QName( NS_WSPM, "refNr" ), refNr ); //$NON-NLS-1$
  }

  public void setDirectionUpstreams( final boolean directionIsUpstream )
  {
    setProperty( new QName( NS_WSPM, "isDirectionUpstream" ), new Boolean( directionIsUpstream ) ); //$NON-NLS-1$
  }

  public Feature createRunOffEvent( ) throws GMLSchemaException
  {
    return FeatureHelper.addFeature( getFeature(), new QName( NS_WSPM, "runOffEventMember" ), new QName( NS_WSPMRUNOFF, "RunOffEvent" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public Feature createWspFix( ) throws GMLSchemaException
  {
    return FeatureHelper.addFeature( getFeature(), QNAME_WSP_FIX_MEMBER, new QName( NS_WSPMRUNOFF, "WaterlevelFixation" ) ); //$NON-NLS-1$
  }

  public List< ? > getWspFixations( )
  {
    return getProperty( QNAME_WSP_FIX_MEMBER, List.class );
  }

  public boolean isDirectionUpstreams( )
  {
    return getProperty( new QName( NS_WSPM, "isDirectionUpstream" ), Boolean.class ); //$NON-NLS-1$
  }

  public WspmReach[] getReaches( )
  {
    final FeatureList reaches = (FeatureList) getFeature().getProperty( QNAME_REACH_MEMBER );
    final List<WspmReach> reachList = new ArrayList<WspmReach>( reaches.size() );
    for( final Object object : reaches )
    {
      final Feature f = (Feature) object;
      reachList.add( new WspmReach( f )
      {
      } );
    }

    return reachList.toArray( new WspmReach[reachList.size()] );
  }
}
