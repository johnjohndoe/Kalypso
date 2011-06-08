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


/**
 * A set of {@link ProfileData}
 * 
 * @author Holger Albert
 */
public class ProfileDataSet
{
  /**
   * The start position (the first station). In meters!
   */
  private final double m_startPosition;

  /**
   * The end position (the last station). In meters!
   */
  private final double m_endPosition;

  private final ProfileData[] m_profileData;

  /**
   * @param startPosition
   *          The start position (the first station). In meters!
   * @param endPosition
   *          The end position (the last station). In meters!
   */
  public ProfileDataSet( final ProfileData[] profileData, final double startPosition, final double endPosition )
  {
    m_profileData = profileData;

    /* Calculate the ranges. */
    final int size = profileData.length;
    for( int i = 0; i < size; i++ )
    {
      final ProfileData data = profileData[i];
      final ProfileData prevProfile = i == 0 || size == 1 ? null : profileData[i - 1];
      final ProfileData nextProfile = i + 1 < size ? profileData[i + 1] : null;
      final double range = data.calculateRange( prevProfile, nextProfile );
      data.setRange( range );
    }

    /* Are there already a start and end position given? */
    if( !Double.isNaN( startPosition ) && !Double.isNaN( endPosition ) )
    {
      m_startPosition = startPosition;
      m_endPosition = endPosition;
      return;
    }
    else if( size > 0 )
    {
      /* Calculate the start and end position. */
      m_startPosition = profileData[0].getPosition();
      m_endPosition = profileData[size - 1].getPosition();
    }
    else
    {
      m_startPosition = Double.NaN;
      m_endPosition = Double.NaN;
    }
  }

  public ProfileData[] getAllProfiles( )
  {
    return m_profileData;
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

  public IKMValue[] getKMValues( final int paramCount )
  {
    final IKMValue[] kmMerged = getMergedParams();

    final int[] qIndices = buildMapping( paramCount );
    final IKMValue[] result = new IKMValue[paramCount];
    for( int i = 0; i < result.length; i++ )
      result[i] = kmMerged[qIndices[i]];
    return result;
  }

  /**
   * Calculates which paramCount-index maps to which q-index.
   */
  private int[] buildMapping( final int paramCount )
  {
    // FIXME: find q-bordvoll (mean of all q bordvolls of all profiles weighted by length)

    // FIXME: find index of q-bordvoll

    // FIXME: separate into two block: half before and half after q bordvoll

    // FIXME: map indices

    final int numberQ = getNumberQ();

    // Calculate for the number of discharges (at the moment always 5)

    // TODO: dubious 2: we interpolate the index ?! Instead we should divide the q-range by 5, and use these q's!
    // Use the 5 discharges as follows: first,(first+middle)/2,middle,middle+last)/2,last
    final int interval = (numberQ - 1) / 4;

    // REMARK: as we just pick existing discharges, interpolation actually makes no sense; maybe, later, if we really
    // pick intermediate discharges, we should interpolate again
    final int[] qIndices = new int[paramCount];
    qIndices[0] = 0;
    qIndices[1] = 1 * interval;
    qIndices[2] = 2 * interval;
    qIndices[3] = 3 * interval;
    qIndices[paramCount - 1] = numberQ - 1;

    return qIndices;
  }

  /**
   * Generates one kmValue for each q by combining for each q all parameters of all profiles.
   */
  private IKMValue[] getMergedParams( )
  {
    final int numKMValues = getNumberQ();
    final IKMValue[] kmMerged = new IKMValue[numKMValues];
    for( int indexQ = 0; indexQ < numKMValues; indexQ++ )
      kmMerged[indexQ] = getKMValue( indexQ );
    return kmMerged;
  }

  protected int getNumberQ( )
  {
    final ProfileData data = m_profileData[0];
    final int numKMValues = data.getNumberKMValues();
    return numKMValues;
  }

  /**
   * This function collects KMValues for one discharge (indexQ) over all profiles.
   * 
   * @param indexQ
   *          The index of the discharge.
   * @return The km values.
   */
  private IKMValue getKMValue( final int indexQ )
  {
    final IKMValue[] profileKMs = new IKMValue[m_profileData.length];

    for( int i = 0; i < profileKMs.length; i++ )
      profileKMs[i] = m_profileData[i].getKMValue( indexQ );

    return new MultiKMValue( profileKMs );
  }
}