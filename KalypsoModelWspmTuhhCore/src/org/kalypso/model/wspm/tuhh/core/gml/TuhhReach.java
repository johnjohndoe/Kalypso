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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.IProfileSelectionProvider;
import org.kalypso.model.wspm.core.gml.WspmProfileComparator;
import org.kalypso.model.wspm.core.gml.WspmProject;
import org.kalypso.model.wspm.core.gml.WspmReach;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
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
public class TuhhReach extends WspmReach implements IWspmConstants, IWspmTuhhConstants, IProfileSelectionProvider
{
  private static final QName QNAME_WATER_BODY_LINK_MEMBER = new QName( NS_WSPM_TUHH, "waterBodyLinkMember" ); //$NON-NLS-1$

  public final static QName QNAME_TUHH_REACH = new QName( NS_WSPM_TUHH, "ReachWspmTuhhSteadyState" ); //$NON-NLS-1$

  private static final QName QNAME_MARKER_MEMBER = new QName( IWspmTuhhConstants.NS_WSPM_TUHH, "markerMember" ); //$NON-NLS-1$

  public static final QName QNAME_PROP_REACHSEGMENTMEMBER = new QName( NS_WSPM_TUHH, "reachSegmentMember" ); //$NON-NLS-1$

  public TuhhReach( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  /**
   * Creates and adds a new profile segment to this reach.
   */
  public TuhhReachProfileSegment createProfileSegment( final IProfileFeature profileReference, final double station )
  {
    try
    {
      final Feature feature = FeatureHelper.addFeature( this, QNAME_PROP_REACHSEGMENTMEMBER, new QName( NS_WSPM_TUHH, "ProfileReachSegmentWspmTuhhSteadyState" ) ); //$NON-NLS-1$
      final TuhhReachProfileSegment tuhhProfilesegment = (TuhhReachProfileSegment) feature;

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
      if( GMLSchemaUtilities.substitutes( segmentFeature.getFeatureType(), new QName( NS_WSPM_TUHH, "ProfileReachSegmentWspmTuhhSteadyState" ) ) ) //$NON-NLS-1$
      {
        final TuhhReachProfileSegment segment = (TuhhReachProfileSegment) segmentFeature;
        final IProfileFeature profileMember = segment.getProfileMember();
        if( profileMember != null )
          profilesegments.add( segment );
      }
    }

    return profilesegments.toArray( new TuhhReachProfileSegment[profilesegments.size()] );
  }

  public void setWaterBody( final WspmWaterBody body )
  {
    final IPropertyType waterProp = getFeatureType().getProperty( QNAME_WATER_BODY_LINK_MEMBER );
    setProperty( waterProp, body.getId() );
  }

  public WspmWaterBody getWaterBody( )
  {
    final Object body = getProperty( QNAME_WATER_BODY_LINK_MEMBER );

    return (WspmWaterBody) FeatureHelper.resolveLinkedFeature( getWorkspace(), body );
  }

  private FeatureList getReachSegmentList( )
  {
    return (FeatureList) getProperty( QNAME_PROP_REACHSEGMENTMEMBER );
  }

  public void recreateMarkerList( )
  {
    final IFeatureType featureType = getFeatureType();
    final IRelationType markerRT = (IRelationType) featureType.getProperty( QNAME_MARKER_MEMBER );

    final FeatureList list = (FeatureList) getProperty( markerRT );
    list.clear();

    final TuhhReachProfileSegment[] reachProfileSegments = getReachProfileSegments();
    for( final TuhhReachProfileSegment segment : reachProfileSegments )
    {
      final IProfileFeature profileMember = segment.getProfileMember();
      if( profileMember == null )
        continue;

      final String crs = profileMember.getSrsName();
      final IProfil profil = profileMember.getProfil();
      final IComponent[] markerTypes = profil.getPointMarkerTypes();
      for( final IComponent markerTyp : markerTypes )
      {
        final IProfilPointMarker[] deviders = profil.getPointMarkerFor( markerTyp );
        for( final IProfilPointMarker devider : deviders )
        {
          try
          {
            final IRecord point = devider.getPoint();
            // REMARK: create the point before the marker because we may have an exception
            final GM_Point location = ProfileCacherFeaturePropertyFunction.convertPoint( profil, point, crs );

            final TuhhMarker marker = createMarker( list );
            list.add( marker.getFeature() );

            marker.setName( markerTyp.getName() );
            marker.setType( markerTyp.getId() );
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
  }

  private TuhhMarker createMarker( final FeatureList markerList )
  {
    final GMLWorkspace workspace = getWorkspace();

    final IRelationType markerRelation = markerList.getParentFeatureTypeProperty();
    final IFeatureType markerFT = markerRelation.getTargetFeatureType();
    final Feature markerFeature = workspace.createFeature( this, markerRelation, markerFT );

    return new TuhhMarker( markerFeature );
  }

  /**
   * @see org.kalypso.model.wspm.core.gml.IProfileSelectionProvider#getSelectedProfiles(org.kalypso.gmlschema.property.relation.IRelationType)
   */
  @Override
  public IProfileFeature[] getSelectedProfiles( final IRelationType selectionHint )
  {
    final FeatureList reachSegmentList = getReachSegmentList();
    final SortedSet<IProfileFeature> profile = new TreeSet<IProfileFeature>( new WspmProfileComparator( isDirectionUpstreams() ) );

    for( final Object object : reachSegmentList )
    {
      final Feature segmentFeature = (Feature) object;
      if( GMLSchemaUtilities.substitutes( segmentFeature.getFeatureType(), new QName( NS_WSPM_TUHH, "ProfileReachSegmentWspmTuhhSteadyState" ) ) ) //$NON-NLS-1$
      {
        final TuhhReachProfileSegment segment = (TuhhReachProfileSegment) segmentFeature;
        final IProfileFeature profileMember = segment.getProfileMember();
        if( profileMember != null )
          profile.add( profileMember );
      }
    }

    return profile.toArray( new IProfileFeature[profile.size()] );
  }

  /**
   * Returns all calculations, that use this reach.
   */
  public TuhhCalculation[] findCalculations( )
  {
    final Collection<TuhhCalculation> result = new ArrayList<TuhhCalculation>();

    final WspmWaterBody waterBody = getWaterBody();
    if( waterBody != null )
    {
      final WspmProject project = waterBody.getProject();
      if( project instanceof TuhhWspmProject )
      {
        final TuhhWspmProject tuhhProject = (TuhhWspmProject) project;
        final TuhhCalculation[] calculations = tuhhProject.getCalculations();
        for( final TuhhCalculation tuhhCalculation : calculations )
        {
          final TuhhReach reach = tuhhCalculation.getReach();
          if( this.equals( reach ) )
            result.add( tuhhCalculation );
        }
      }
    }

    return result.toArray( new TuhhCalculation[result.size()] );
  }

  public IProfileFeature findProfile( final BigDecimal station )
  {
    final TuhhReachProfileSegment[] segments = getReachProfileSegments();
    for( final TuhhReachProfileSegment segment : segments )
    {
      if( station.equals( segment.getStation() ) )
        return segment.getProfileMember();
    }

    return null;
  }

  public boolean isDirectionUpstreams( )
  {
    return getWaterBody().isDirectionUpstreams();
  }
}