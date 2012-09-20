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
package org.kalypso.model.wspm.tuhh.core.profile.importer.wprof;

import java.math.BigDecimal;
import java.util.Iterator;
import java.util.SortedMap;
import java.util.TreeMap;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.commons.math.LinearEquation;
import org.kalypso.commons.math.LinearEquation.SameXValuesException;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.visitors.ProfileVisitors;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class KreisOWCreator implements IProfileSecondaryCreator, IWspmTuhhConstants
{
  private final ProfileData[] m_data;

  public KreisOWCreator( final ProfileData[] data )
  {
    m_data = data;
  }

  @Override
  public void execute( final TuhhWspmProject project, final ProfileData[] data ) throws CoreException
  {
    for( final ProfileData profileData : data )
    {
      final IProfileCreator creator = profileData.getProfileCreator();
      if( creator instanceof KreisProfileCreator )
      {
        createKreisOW( project, profileData );
      }
    }
  }

  private void createKreisOW( final TuhhWspmProject project, final ProfileData kreisData ) throws CoreException
  {
    final IProfile kreisProfile = kreisData.getProfile();

    final GelaendeProfileCreator kreisOWCreator = createGelaendeCreator( kreisData, kreisProfile );
    if( kreisOWCreator == null )
      return;

    kreisOWCreator.setCreateLocation( false );
    kreisOWCreator.addProfile( project );
  }

  private GelaendeProfileCreator createGelaendeCreator( final ProfileData kreisData, final IProfile kreisProfile )
  {
    final double minDistance = 10.0;
    final double owDistance = 6.0;

    final double kreisStation = kreisProfile.getStation();
    if( Double.isNaN( kreisStation ) )
      return null;

    final String riverId = kreisData.getRiverId();
    final IProfile owProfile = findOWProfile( riverId, kreisStation );
    final double owStation = owProfile.getStation();
    if( Double.isNaN( owStation ) )
      return null;

    final double distance = Math.abs( kreisStation - owStation );
    final double signum = Math.signum( kreisStation - owStation );
    if( distance < minDistance )
      return null;

    final double kreisOwStation = kreisStation + signum * owDistance;

    final double kreisSoil = ProfileVisitors.findLowestPoint( kreisProfile ).getBreite();
    final double owSoil = ProfileVisitors.findLowestPoint( owProfile ).getBreite();

    final double kreisOwSoil = interpolateSoil( kreisSoil, kreisStation, owSoil, owStation, kreisOwStation );
    if( Double.isNaN( kreisOwSoil ) )
      return null;

    final double offset = Math.max( 0.0, kreisOwSoil - kreisSoil );

    final GelaendeProfileCreator kreisOWCreator = new GelaendeProfileCreator( Messages.getString( "KreisOWCreator_0" ), kreisData, "V01" ); //$NON-NLS-1$//$NON-NLS-2$

    kreisOWCreator.setSoilOffset( offset );
    kreisOWCreator.setOverwriteStation( new BigDecimal( kreisOwStation ).setScale( STATION_SCALE, BigDecimal.ROUND_HALF_UP ) );

    return kreisOWCreator;
  }

  private double interpolateSoil( final double kreisSoil, final double kreisStation, final double owSoil, final double owStation, final double kreisOwStation )
  {
    try
    {
      final LinearEquation linearEquation = new LinearEquation( kreisStation, kreisSoil, owStation, owSoil );
      return linearEquation.computeY( kreisOwStation );
    }
    catch( final SameXValuesException e )
    {
      e.printStackTrace();
      return Double.NaN;
    }
  }

  private IProfile findOWProfile( final String riverId, final double uwStation )
  {
    final SortedMap<Double, IProfile> profilesOfRiver = indexProfilesForRiver( riverId );

    // FIXME: Stationierungsrichtung beachten!
    final SortedMap<Double, IProfile> tailMap = profilesOfRiver.tailMap( uwStation );
    final Iterator<IProfile> valuesIter = tailMap.values().iterator();
    if( valuesIter.hasNext() )
      return valuesIter.next();

    return null;
  }

  private SortedMap<Double, IProfile> indexProfilesForRiver( final String riverId )
  {
    final SortedMap<Double, IProfile> profilesOfRiver = new TreeMap<>();
    for( final ProfileData data : m_data )
    {
      final String dataRiver = data.getRiverId();
      if( riverId.equals( dataRiver ) )
      {
        final IProfile profile = data.getProfile();
        final double station = profile.getStation();
        profilesOfRiver.put( station, profile );
      }
    }

    return profilesOfRiver;
  }
}
