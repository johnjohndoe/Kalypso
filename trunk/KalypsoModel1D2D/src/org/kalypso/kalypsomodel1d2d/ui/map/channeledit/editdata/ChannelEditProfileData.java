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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.SIDE;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypsodeegree.model.geometry.GM_Curve;

/**
 * Encapsulates all profile data the user works on for channel editing:
 * <ul>
 * <li>the selected profiles</li>
 * <li>the segments</li>
 * </ul>
 *
 * @author Gernot Belger
 */
public class ChannelEditProfileData
{
  /* null means: data not initalized */
  private List<SegmentData> m_segments = null;

  private final IProfileData[] m_profiles;

  private final Map<GM_Curve, SIDE> m_banks;

  private final int m_numberBankIntersection;

  private final int m_numberProfileIntersection;

  public ChannelEditProfileData( final IProfileFeature[] profiles, final int numberProfileIntersection, final Map<GM_Curve, SIDE> banks, final int numberBankIntersection )
  {
    m_profiles = ProfileData.from( profiles, numberProfileIntersection );
    m_numberProfileIntersection = numberProfileIntersection;

    m_banks = banks;
    m_numberBankIntersection = numberBankIntersection;

    /* sort by station */
    // Sort so that stations are in ascending order; this is nice in the combo viewer.
    final ProfileDataStationComparator comparator = new ProfileDataStationComparator( true );
    Arrays.sort( m_profiles, comparator );
  }

  public IProfileData[] getProfiles( )
  {
    return m_profiles;
  }

  void initData( )
  {
    m_segments = new ArrayList<>();

    // there must be at least two selected profiles and one selected bank.

    // FIXME: we need one bank PER SIDE: this is not guarantueed by this code
    // TODO: works only, because we only keep exactly one bank per side
    if( m_banks.size() < 2 )
      return;

    //monitor.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.9" ), profiles.length ); //$NON-NLS-1$

    // loop over all profiles
    // take two neighbouring profiles create a segment for them

    IProfileData lastProfile = null;
    for( final IProfileData profile : m_profiles )
    {
      //monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.10" ) + profile.getStation() ); //$NON-NLS-1$

      if( !checkInteIntersectionWithBanks( profile ) )
        continue;

      if( lastProfile != null )
      {
        final ProfileData upProfile = (ProfileData)profile;
        final ProfileData downProfile = (ProfileData)lastProfile;

        final SegmentData segment = new SegmentData( upProfile, downProfile, m_banks, m_numberBankIntersection );
        m_segments.add( segment );
      }

      lastProfile = profile;

      // if( monitor.isCanceled() )
      // throw new OperationCanceledException();

      // monitor.worked( 1 );
    }

    /**
     * Intersects the two WSPM profiles (upstream/downstream) with the allready intersected bank lines (left/right) of
     * the current segment.<br>
     * The two profiles will be cropped at the intersection points and intersected by a specific number of points. <br>
     * Afterwards an area adjustment for the intersected profiles will be done .
     */
    // FIXME: better management of update needed
    for( final IProfileData profile : m_profiles )
      ((ProfileData)profile).recalculateSegmentedProfile();

    for( final SegmentData segment : m_segments )
      segment.updateMesh();
  }

  private boolean checkInteIntersectionWithBanks( final IProfileData profile )
  {
    final Set<GM_Curve> curves = m_banks.keySet();

    // TODO: bad test: profile should intersect one left-bank and one right-bank
    // TODO: only works, because we have exactly one left and one right bank

    for( final GM_Curve curve : curves )
    {
      final GM_Curve line = profile.getFeature().getLine();

      if( !curve.intersects( line ) )
        return false;
    }

    return true;
  }

  public IProfileData[] findProfiles( final IProfileFeature[] features )
  {
    final Set<IProfileFeature> featureSet = new HashSet<>( Arrays.asList( features ) );

    final Collection<IProfileData> foundData = new ArrayList<>();
    for( final IProfileData data : m_profiles )
    {
      if( featureSet.contains( data.getFeature() ) )
        foundData.add( data );
    }

    return foundData.toArray( new ProfileData[foundData.size()] );
  }

  public GM_Curve getBanklineForSide( final SIDE side )
  {
    for( final Map.Entry<GM_Curve, SIDE> entry : m_banks.entrySet() )
    {
      final GM_Curve bankCurve = entry.getKey();
      final SIDE value = entry.getValue();
      if( value == side )
        return bankCurve;
    }

    return null;
  }

  public Map<GM_Curve, SIDE> getBanklines( )
  {
    return m_banks;
  }

  public int getNumberProfileSegments( )
  {
    return m_numberProfileIntersection;
  }

  public int getNumberBanklineSegments( )
  {
    return m_numberBankIntersection;
  }

  public IProfileFeature[] getProfileFeatures( )
  {
    final IProfileFeature[] features = new IProfileFeature[m_profiles.length];

    for( int i = 0; i < features.length; i++ )
      features[i] = m_profiles[i].getFeature();

    return features;
  }
}