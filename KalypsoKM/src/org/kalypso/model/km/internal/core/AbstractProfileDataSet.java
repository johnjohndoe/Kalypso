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
package org.kalypso.model.km.internal.core;

import java.util.SortedSet;
import java.util.TreeSet;

/**
 * Base implementation of the profile data set.
 * 
 * @author Holger Albert
 */
public abstract class AbstractProfileDataSet
{
  /**
   * The sorted profile data.
   */
  private SortedSet<ProfileData> m_profileSort;

  /**
   * The start position (the first station). In meters!
   */
  private double m_startPosition;

  /**
   * The end position (the last station). In meters!
   */
  private double m_endPosition;

  /**
   * The constructor.
   * 
   * @param startPosition
   *          The start position (the first station). In meters!
   * @param endPosition
   *          The end position (the last station). In meters!
   */
  public AbstractProfileDataSet( double startPosition, double endPosition )
  {
    m_profileSort = new TreeSet<ProfileData>( new ProfileDataComparator() );
    m_startPosition = startPosition;
    m_endPosition = endPosition;
  }

  public void init( )
  {
    /* Create and add the profile data. */
    createProfileData();

    /* Calculate the ranges. */
    ProfileData[] profiles = m_profileSort.toArray( new ProfileData[m_profileSort.size()] );
    for( int i = 0; i < profiles.length; i++ )
    {
      ProfileData data = profiles[i];
      ProfileData prevProfile = i == 0 || profiles.length == 1 ? null : profiles[i - 1];
      ProfileData nextProfile = i + 1 < profiles.length ? profiles[i + 1] : null;
      double range = data.calculateRange( prevProfile, nextProfile );
      data.setRange( range );
    }

    /* Are there already a start and end position given? */
    if( !Double.isNaN( m_startPosition ) && !Double.isNaN( m_endPosition ) )
      return;

    /* Calculate the start and end position. */
    m_startPosition = Double.NaN;
    m_endPosition = Double.NaN;
    if( m_profileSort.size() > 0 )
    {
      m_startPosition = m_profileSort.first().getPosition();
      m_endPosition = m_profileSort.last().getPosition();
    }
  }

  /**
   * Create and add (calling {@link #addProfileData(ProfileData)}) your profile data in this function.
   */
  protected abstract void createProfileData( );

  protected void addProfileData( ProfileData profileData )
  {
    if( !Double.isNaN( m_startPosition ) && !Double.isNaN( m_endPosition ) )
    {
      double profilePos = profileData.getPosition();
      if( m_startPosition <= profilePos && profilePos <= m_endPosition )
        m_profileSort.add( profileData );
    }
    else
      m_profileSort.add( profileData );
  }

  public ProfileData[] getAllProfiles( )
  {
    return m_profileSort.toArray( new ProfileData[m_profileSort.size()] );
  }

  /**
   * This function returns the start position (the first station). In meters!
   * 
   * @return The start position (the first station). In meters!
   */
  public double getStartPosition( )
  {
    return m_startPosition;
  }

  /**
   * This function returns the end position (the last station). In meters!
   * 
   * @return The end position (the last station). In meters!
   */
  public double getEndPosition( )
  {
    return m_endPosition;
  }

  public IKMValue[] getKMValues( )
  {
    ProfileData data = m_profileSort.first();

    // generate kmValues for each q;
    int numKMValues = data.getNumberKMValues();
    IKMValue[] kmMerged = new IKMValue[numKMValues];
    for( int index = 0; index < numKMValues; index++ )
    {
      IKMValue kmValue = getKMValue( index );
      kmMerged[index] = kmValue;
    }

    SortedSet<IKMValue> sort = new TreeSet<IKMValue>( new KMValueComparator() );
    for( IKMValue element : kmMerged )
      sort.add( element );

    // FIXME: looks not natural we just pick discharges by chance (fixed interval length); shoudn't we combine the
    // intermediate values?

    // Calculate for the number of discharges (at the moment always 5)
    IKMValue kmFirst = kmMerged[0];
    IKMValue kmLast = kmMerged[kmMerged.length - 1];

    // TODO: dubious 2: we interpolate the index ?! Instead we should devide the q-range by 5, and use these q's!

    // Use the 5 discharges as follows: first,(first+middle)/2,middle,middle+last)/2,last
    int interval = (kmMerged.length - 1) / 4;

    IKMValue[] result = new IKMValue[5];
    // REMARK: as we just pick existing discharges, interpolation actually makes no sense; maybe, later, if we really
    // pick intermediate discharges, we should interpolate again
    result[0] = kmFirst;
    result[1] = kmMerged[1 * interval];
    result[2] = kmMerged[2 * interval];
    result[3] = kmMerged[3 * interval];
    result[4] = kmLast;
    // result[0] = KMValueFromQinterpolation.interpolateKM( sort, kmFirst.getQSum() );
    // result[1] = KMValueFromQinterpolation.interpolateKM( sort, kmMerged[1 * interval].getQSum() );
    // result[2] = KMValueFromQinterpolation.interpolateKM( sort, kmMerged[2 * interval].getQSum() );
    // result[3] = KMValueFromQinterpolation.interpolateKM( sort, kmMerged[3 * interval].getQSum() );
    // result[4] = KMValueFromQinterpolation.interpolateKM( sort, kmLast.getQSum() );

    return result;
  }

  /**
   * This function collects KMValues for one index (row) over all profiles.
   * 
   * @param index
   *          The index.
   * @return The km values.
   */
  private IKMValue getKMValue( int index )
  {
    ProfileData[] profiles = m_profileSort.toArray( new ProfileData[m_profileSort.size()] );
    IKMValue[] profileKMs = new IKMValue[profiles.length];

    for( int i = 0; i < profileKMs.length; i++ )
      profileKMs[i] = profiles[i].getKMValue( index );

    return new MultiKMValue( profileKMs );
  }
}