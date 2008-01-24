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
package org.kalypso.model.wspm.tuhh.core.gml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.gml.WspmReach;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.schema.gml.ProfileCacherFeaturePropertyFunction;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public class TuhhReach extends WspmReach implements IWspmConstants, IWspmTuhhConstants
{
  private static final QName QNAME_WATER_BODY_LINK_MEMBER = new QName( NS_WSPM_TUHH, "waterBodyLinkMember" );

  public final static QName QNAME_TUHH_REACH = new QName( NS_WSPM_TUHH, "ReachWspmTuhhSteadyState" );

  private static final QName QNAME_MARKER_MEMBER = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "markerMember" );

  public static final QName QNAME_PROP_REACHSEGMENTMEMBER = new QName( NS_WSPM_TUHH, "reachSegmentMember" );

  public TuhhReach( final Feature reach )
  {
    super( reach, QNAME_TUHH_REACH );
  }

  /**
   * Creates and adds a new profile segment to this reach.
   */
  public TuhhReachProfileSegment createProfileSegment( final WspmProfile profileReference, final double station )
  {
    try
    {
      final Feature feature = FeatureHelper.addFeature( getFeature(), QNAME_PROP_REACHSEGMENTMEMBER, new QName( NS_WSPM_TUHH, "ProfileReachSegmentWspmTuhhSteadyState" ) );
      final TuhhReachProfileSegment tuhhProfilesegment = new TuhhReachProfileSegment( feature );

      // set default values
      tuhhProfilesegment.setProfileMember( profileReference );
      tuhhProfilesegment.setStation( station );

      return tuhhProfilesegment;
    }
    catch( final GMLSchemaException e )
    {
      // should never happen
      e.printStackTrace();

      return null;
    }

  }

  public TuhhReachProfileSegment[] getReachProfileSegments( )
  {
    final FeatureList reachSegmentList = getReachSegmentList();
    final List<TuhhReachProfileSegment> profilesegments = new ArrayList<TuhhReachProfileSegment>();
    for( final Object object : reachSegmentList )
    {
      final Feature segmentFeature = (Feature) object;
      if( GMLSchemaUtilities.substitutes( segmentFeature.getFeatureType(), new QName( NS_WSPM_TUHH, "ProfileReachSegmentWspmTuhhSteadyState" ) ) )
      {
        final TuhhReachProfileSegment segment = new TuhhReachProfileSegment( segmentFeature );
        final WspmProfile profileMember = segment.getProfileMember();
        if( profileMember != null )
          profilesegments.add( segment );
      }
    }

    return profilesegments.toArray( new TuhhReachProfileSegment[profilesegments.size()] );
  }

  public void setWaterBody( final WspmWaterBody body )
  {
    final IPropertyType waterProp = getFeature().getFeatureType().getProperty( QNAME_WATER_BODY_LINK_MEMBER );
    getFeature().setProperty( waterProp, body.getFeature().getId() );
  }

  public WspmWaterBody getWaterBody( )
  {
    final Object body = getFeature().getProperty( QNAME_WATER_BODY_LINK_MEMBER );
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
    return (FeatureList) getFeature().getProperty( QNAME_PROP_REACHSEGMENTMEMBER );
  }

  public void recreateMarkerList( )
  {
    final Feature feature = getFeature();
    final IFeatureType featureType = feature.getFeatureType();
    final IRelationType markerRT = (IRelationType) featureType.getProperty( QNAME_MARKER_MEMBER );

    final FeatureList list = (FeatureList) feature.getProperty( markerRT );
    list.clear();

    final TuhhReachProfileSegment[] reachProfileSegments = getReachProfileSegments();
    for( final TuhhReachProfileSegment segment : reachProfileSegments )
    {
      final WspmProfile profileMember = segment.getProfileMember();
      if( profileMember == null )
        continue;

      final IProfil profil = profileMember.getProfil();
      final IProfilPointMarker[] deviders = profil.getPointMarkerFor( ProfilObsHelper.getPropertyFromId( profil, profil.getType() ) );
      for( final IProfilPointMarker devider : deviders )
      {
        try
        {
          final IComponent typ = devider.getId();
          final IRecord point = devider.getPoint();
          // REMARK: create the point before the marker because we may have an exception
          final GM_Point location = ProfileCacherFeaturePropertyFunction.convertPoint( profil, point );

          final TuhhMarker marker = createMarker( list );
          list.add( marker.getFeature() );

          marker.setName( typ.getName() );
          marker.setType( typ.getId() );
          marker.setLocation( location );

        }
        catch( final Exception e )
        {
          final IStatus status = StatusUtilities.statusFromThrowable( e );
          KalypsoModelWspmTuhhCorePlugin.getDefault().getLog().log( status );
        }
      }
    }
  }

  private TuhhMarker createMarker( final FeatureList markerList )
  {
    final Feature feature = getFeature();
    final GMLWorkspace workspace = feature.getWorkspace();

    final IRelationType markerRelation = markerList.getParentFeatureTypeProperty();
    final IFeatureType markerFT = markerRelation.getTargetFeatureType();
    final Feature markerFeature = workspace.createFeature( feature, markerRelation, markerFT );

    return new TuhhMarker( markerFeature );
  }

}