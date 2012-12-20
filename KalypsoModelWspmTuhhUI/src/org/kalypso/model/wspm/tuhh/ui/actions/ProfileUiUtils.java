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
package org.kalypso.model.wspm.tuhh.ui.actions;

import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.ProfileFeatureBinding;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointPropertyProvider;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * Various helper code for dealing with profiles, should be used mainly from commands and widgets.
 * 
 * @author Gernot Belger
 */
public final class ProfileUiUtils
{
  private ProfileUiUtils( )
  {
    throw new UnsupportedOperationException();
  }

  public static TuhhReach findReach( final Feature container )
  {
    if( container instanceof TuhhReach )
      return (TuhhReach) container;

    return null;
  }

  public static WspmWaterBody findWaterbody( final Feature container )
  {
    if( container instanceof WspmWaterBody )
      return (WspmWaterBody) container;

    if( container instanceof TuhhReach )
      return ((TuhhReach) container).getWaterBody();

    return null;
  }

  /**
   * Adds two markers of the given type to start and end points of a profile.
   */
  public static void addDefaultMarkers( final IProfile profile, final String markerTyp )
  {
    final IProfilePointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profile.getType() );
    final IProfileRecord[] points = profile.getPoints();
    if( points.length > 0 )
    {
      final Object defaultValue = provider.getDefaultValue( markerTyp );
      profile.createPointMarker( markerTyp, points[0] ).setValue( defaultValue );
      profile.createPointMarker( markerTyp, points[points.length - 1] ).setValue( defaultValue );
    }
  }

  /**
   * @param previous
   *          The new profile segment (if we have a reach) will be inserted after that profile.
   * @return Returns the element that should be selected after the new profile is created. The new reach segment if the
   *         reach is not null.
   */
  public static Feature addNewProfileAndFireEvents( final IProfile iProfile, final WspmWaterBody waterBody, final TuhhReach reach, final IProfileFeature previous )
  {
    final IProfileFeature feature = waterBody.createNewProfile();
    ((ProfileFeatureBinding) feature).setProfile( iProfile );

    /* Move feature to right place */
    final IFeatureBindingCollection<IProfileFeature> profiles = waterBody.getProfiles();
    final int previousProfileIndex = previous == null ? -1 : profiles.indexOf( previous );
    if( previousProfileIndex != -1 )
    {
      profiles.remove( profiles.size() - 1 );
      profiles.add( previousProfileIndex + 1, feature );
    }

    final GMLWorkspace workspace = waterBody.getWorkspace();
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, waterBody, new Feature[] { feature }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    if( reach != null )
    {
      final TuhhReachProfileSegment segment = reach.createProfileSegment( feature, iProfile.getStation() );

      /* insert at the right position */
      final FeatureList reachSegmentList = reach.getReachSegmentList();
      final int previousSegmentIndex = findSegmentIndex( reachSegmentList, previous );
      if( previousSegmentIndex != -1 )
      {
        reachSegmentList.remove( reachSegmentList.size() - 1 );
        reachSegmentList.add( previousSegmentIndex + 1, segment );
      }

      workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, reach, new Feature[] { segment }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      return segment;
    }

    return feature;
  }

  private static int findSegmentIndex( final FeatureList reachSegmentList, final IProfileFeature previousProfile )
  {
    if( previousProfile == null )
      return -1;

    for( int i = 0; i < reachSegmentList.size(); i++ )
    {
      final TuhhReachProfileSegment segment = (TuhhReachProfileSegment) reachSegmentList.get( i );
      if( segment.getProfileMember() == previousProfile )
        return i;
    }

    return -1;
  }

}
