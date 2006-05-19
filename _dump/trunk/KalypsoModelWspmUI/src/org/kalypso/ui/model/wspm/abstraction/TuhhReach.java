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
package org.kalypso.ui.model.wspm.abstraction;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.contribs.javax.xml.namespace.QNameUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ui.model.wspm.IWspmConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author belger
 */
public class TuhhReach extends WspmReach implements IWspmConstants
{
  public TuhhReach( final Feature reach )
  {
    super( reach );

    if( !QNameUtilities.equals( reach.getFeatureType().getQName(), NS_WSPM_TUHH, "ReachWspmTuhhSteadyState" ) )
      throw new IllegalStateException( "Feature is of wrong type: " + reach );
  }

  /**
   * Creates and adds a new profile segment to this reach.
   */
  public TuhhReachProfileSegment createProfileSegment( final WspmProfileReference profileReference, final double station, final double distanceL, final double distanceM, final double distanceR )
  {
    final FeatureList reachSegmentList = getReachSegmentList();
    final Feature parentFeature = reachSegmentList.getParentFeature();
    final GMLWorkspace workspace = parentFeature.getWorkspace();

    final IRelationType parentFeatureTypeProperty = reachSegmentList.getParentFeatureTypeProperty();

    final IFeatureType targetFeatureType = parentFeatureTypeProperty.getTargetFeatureType();

    final IFeatureType tuhhProfileReachSegmentType = workspace.getGMLSchema().getFeatureType( new QName( NS_WSPM_TUHH, "ProfileReachSegmentWspmTuhhSteadyState" ) );

    final Feature feature = workspace.createFeature( parentFeature, tuhhProfileReachSegmentType );

    reachSegmentList.add( feature );

    final TuhhReachProfileSegment tuhhProfilesegment = new TuhhReachProfileSegment( feature );

    // set default values
    tuhhProfilesegment.setProfileMember( profileReference );
    tuhhProfilesegment.setStation( station );
    tuhhProfilesegment.setDistanceL( distanceL );
    tuhhProfilesegment.setDistanceM( distanceM );
    tuhhProfilesegment.setDistanceR( distanceR );

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
}
