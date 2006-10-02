/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.tuhh.core.gml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.gml.WspmReach;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author belger
 */
public class TuhhReach extends WspmReach implements IWspmConstants, IWspmTuhhConstants
{
  public final static QName QNAME_TUHH_REACH = new QName( NS_WSPM_TUHH, "ReachWspmTuhhSteadyState" );

  public TuhhReach( final Feature reach )
  {
    super( reach, QNAME_TUHH_REACH );
  }

  /**
   * Creates and adds a new profile segment to this reach.
   */
  public TuhhReachProfileSegment createProfileSegment( final WspmProfile profileReference, final double station, final double distanceL, final double distanceM, final double distanceR ) throws GMLSchemaException
  {
    final Feature feature = FeatureHelper.addFeature( getFeature(), new QName( NS_WSPM_TUHH, "reachSegmentMember" ), new QName( NS_WSPM_TUHH, "ProfileReachSegmentWspmTuhhSteadyState" ) );

    final TuhhReachProfileSegment tuhhProfilesegment = new TuhhReachProfileSegment( feature );

    // set default values
    tuhhProfilesegment.setProfileMember( profileReference );
    tuhhProfilesegment.setStation( station );
    // tuhhProfilesegment.setDistanceL( distanceL );
    // tuhhProfilesegment.setDistanceM( distanceM );
    // tuhhProfilesegment.setDistanceR( distanceR );

    return tuhhProfilesegment;
  }

  public TuhhReachProfileSegment[] getReachProfileSegments( )
  {
    final FeatureList reachSegmentList = getReachSegmentList();
    final List<TuhhReachProfileSegment> profilesegments = new ArrayList<TuhhReachProfileSegment>();
    for( final Object object : reachSegmentList )
    {
      final Feature segment = (Feature) object;
      if( GMLSchemaUtilities.substitutes( segment.getFeatureType(), new QName( NS_WSPM_TUHH, "ProfileReachSegmentWspmTuhhSteadyState" ) ) )
        profilesegments.add( new TuhhReachProfileSegment( segment ) );
    }

    return profilesegments.toArray( new TuhhReachProfileSegment[profilesegments.size()] );
  }

  public void setWaterBody( final WspmWaterBody body )
  {
    final IPropertyType waterProp = getFeature().getFeatureType().getProperty( new QName( NS_WSPM_TUHH, "waterBodyLinkMember" ) );
    getFeature().setProperty( waterProp, body.getFeature().getId() );
  }

  public WspmWaterBody getWaterBody( )
  {
    final Object body = getFeature().getProperty( new QName( NS_WSPM_TUHH, "waterBodyLinkMember" ) );
    if( body instanceof Feature )
      return new WspmWaterBody( (Feature) body );
    else
    {
      final Feature feature = getFeature().getWorkspace().getFeature( (String) body );
      return new WspmWaterBody( feature );
    }
  }

  protected FeatureList getReachSegmentList( )
  {
    return (FeatureList) getFeature().getProperty( new QName( NS_WSPM_TUHH, "reachSegmentMember" ) );
  }

}
