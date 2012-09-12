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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.Assert;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.ChannelEditData.SIDE;
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
public class ChannelMesh
{
  /* null means: data not initalized */
  private SegmentData[] m_segments = null;

  private final IProfileData[] m_profiles;

  private final Map<GM_Curve, SIDE> m_banks;

  private final int m_numberBankPoints;

  private final int m_numberProfilePoints;

  public ChannelMesh( final IProfileFeature[] profiles, final int numberProfileIntersection, final Map<GM_Curve, SIDE> banks, final int numberBankIntersection )
  {
    m_profiles = ProfileData.from( profiles, numberProfileIntersection );
    m_numberProfilePoints = numberProfileIntersection;

    m_banks = banks;
    m_numberBankPoints = numberBankIntersection;

    /* Sort so that stations are in ascending order; this is nice in the combo viewer. */
    final ProfileDataStationComparator comparator = new ProfileDataStationComparator();
    Arrays.sort( m_profiles, comparator );
  }

  public IProfileData[] getProfiles( )
  {
    return m_profiles;
  }

  void setSegments( final SegmentData[] segments )
  {
    m_segments = segments;

    /* special case, we do not have any segments (mising ban klines) */
    if( segments.length == 0 )
      return;

    /* check consistency */
    Assert.isTrue( segments.length == m_profiles.length - 1 );

    for( int i = 0; i < segments.length; i++ )
    {
      final SegmentData segment = segments[i];

      final ProfileData profileDown = segment.getProfileDown();
      final ProfileData profileUp = segment.getProfileUp();

      Assert.isTrue( profileDown == m_profiles[i] );
      Assert.isTrue( profileUp == m_profiles[i + 1] );
    }
  }

  public IProfileData[] findProfiles( final String[] ids )
  {
    final Set<String> featureSet = new HashSet<>( Arrays.asList( ids ) );

    final Collection<IProfileData> foundData = new ArrayList<>();
    for( final IProfileData data : m_profiles )
    {
      if( featureSet.contains( data.getId() ) )
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

  public int getNumberProfilePoints( )
  {
    return m_numberProfilePoints;
  }

  public int getNumberBanklinePoints( )
  {
    return m_numberBankPoints;
  }

  public String[] getProfileIDs( )
  {
    final String[] ids = new String[m_profiles.length];

    for( int i = 0; i < ids.length; i++ )
      ids[i] = m_profiles[i].getId();

    return ids;
  }

  public IProfileFeature[] getProfileFeatures( )
  {
    final IProfileFeature[] features = new IProfileFeature[m_profiles.length];

    for( int i = 0; i < features.length; i++ )
      features[i] = ((ProfileData)m_profiles[i]).getFeature();

    return features;
  }

  ISegmentData[] getSegments( )
  {
    return m_segments;
  }
}