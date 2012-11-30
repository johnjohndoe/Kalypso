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
package org.kalypso.model.wspm.tuhh.core.profile.export;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResult;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultInterpolationProfile;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLengthSection;

/**
 * @author Gernot Belger
 */
public class ResultProfileInterpolator
{
  private final IProfileFeature[] m_profiles;

  private final IWspmResult[] m_results;

  private final boolean m_doInterpolation;

  private final boolean m_interpolateForeland;

  public ResultProfileInterpolator( final IProfileFeature[] profiles, final IWspmResult[] results, final boolean doInterpolation, final boolean interpolateForeland )
  {
    m_profiles = profiles;
    m_results = results;
    m_doInterpolation = doInterpolation;
    m_interpolateForeland = interpolateForeland;
  }

  public IProfileFeature[] execute( )
  {
    final Collection<IProfileFeature> allProfiles = new ArrayList<>( (int)(m_profiles.length * 1.1) );

    allProfiles.addAll( Arrays.asList( m_profiles ) );

    for( final IWspmResult result : m_results )
    {
      final IProfileFeature[] interpolatedProfiles = createInterpolatedProfiles( result );
      allProfiles.addAll( Arrays.asList( interpolatedProfiles ) );
    }

    return allProfiles.toArray( new IProfileFeature[allProfiles.size()] );
  }

  private IProfileFeature[] createInterpolatedProfiles( final IWspmResult result )
  {
    if( !m_doInterpolation )
      return new IProfileFeature[0];

    final Collection<IProfileFeature> interpolatedProfiles = new ArrayList<>();

    final TuhhCalculation calculation = result.getCalculation();
    final TuhhReach reach = calculation.getReach();
    final WspmResultLengthSection lengthSection = result.getLengthSection();

    final WspmResultInterpolationProfile[] interpolationProfiles = lengthSection.findInterpolationStations();
    for( final WspmResultInterpolationProfile interpolationProfile : interpolationProfiles )
    {
      final IProfileFeature interpolatedProfile = interpolationProfile.createInterpolatedProfile( reach, !m_interpolateForeland );
      interpolatedProfiles.add( interpolatedProfile );
    }

    return interpolatedProfiles.toArray( new IProfileFeature[interpolatedProfiles.size()] );
  }

}
