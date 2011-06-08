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
    final int qBordvollIndex = findQBordvoll( kmMerged );

    final int[] qIndices = buildMapping( paramCount, qBordvollIndex );
    final IKMValue[] result = new IKMValue[paramCount];
    for( int i = 0; i < result.length; i++ )
      result[i] = kmMerged[qIndices[i]];
    return result;
  }

  private int findQBordvoll( final IKMValue[] kmMerged )
  {
    for( int i = 0; i < kmMerged.length; i++ )
    {
      if( kmMerged[i].getKForeland() > 0 )
        return i;
    }

    return -1;
  }

  /**
   * Calculates which paramCount-index maps to which q-index.
   */
  private int[] buildMapping( final int paramCount, final int qBordvollIndex )
  {
    final int numberQ = getNumberQ();
    final int maxIndexQ = numberQ - 1;

    final int middleParamIndex = (int) ((paramCount / 2.0f));

    final int[] qIndices = new int[paramCount];

    buildMapping( qIndices, 0, middleParamIndex, 0, qBordvollIndex );
    buildMapping( qIndices, middleParamIndex, paramCount, qBordvollIndex, numberQ );

    qIndices[paramCount - 1] = maxIndexQ;

    return qIndices;
  }

  private void buildMapping( final int[] qIndices, final int paramFrom, final int paramTo, final int qIndexFrom, final int qIndexTo )
  {
    final double interval = ((double) qIndexTo - qIndexFrom - 1) / (paramTo - paramFrom - 1);

    for( int i = 0; i < paramTo - paramFrom; i++ )
      qIndices[i + paramFrom] = qIndexFrom + (int) (i * interval);
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