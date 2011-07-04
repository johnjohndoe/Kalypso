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

import java.math.BigDecimal;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.km.internal.KMPlugin;
import org.kalypso.model.km.internal.i18n.Messages;

/**
 * A set of {@link ProfileData}
 * 
 * @author Holger Albert
 */
public class ProfileDataSet
{
  private final ProfileData[] m_profileData;

  public ProfileDataSet( final ProfileData[] profileData )
  {
    m_profileData = profileData;
  }

  void calculateProfileLengths( )
  {
    /* Calculate the ranges. */
    final int size = m_profileData.length;
    for( int i = 0; i < size; i++ )
    {
      final ProfileData profile = m_profileData[i];
      final ProfileData prevProfile = i == 0 || size == 1 ? null : m_profileData[i - 1];
      final ProfileData nextProfile = i + 1 < size ? m_profileData[i + 1] : null;
      final double length = calculateLength( profile, prevProfile, nextProfile );
      profile.setLength( length );
    }
  }

  private double calculateLength( final ProfileData profile, final ProfileData prevProfile, final ProfileData nextProfile )
  {
    final BigDecimal station = profile.getStation();

    /*
     * Distinguish profiles with identical stations from profile sets with one single profile (the latter gets NaN, the
     * first case 0.0)
     */
    if( nextProfile == null && prevProfile == null )
      return Double.NaN;

    final double resultMax = findHalfLength( station, nextProfile );
    final double resultMin = findHalfLength( station, prevProfile );

    return (resultMax - resultMin) * 1000.0;
  }

  private double findHalfLength( final BigDecimal station, final ProfileData nextProfile )
  {
    if( nextProfile == null )
      return station.doubleValue();

    final BigDecimal nextStation = nextProfile.getStation();
    return (station.doubleValue() + nextStation.doubleValue()) / 2d;
  }

  public double getQBordvoll( )
  {
    /* Only in this case we have one degenerate profile with length = NaN */
    if( m_profileData.length == 1 )
      return m_profileData[0].getLength();

    final double[] qBordvoll = new double[m_profileData.length];
    final double[] length = new double[m_profileData.length];
    for( int i = 0; i < m_profileData.length; i++ )
    {
      final ProfileData profileData = m_profileData[i];
      qBordvoll[i] = profileData.findQBordvoll();
      length[i] = profileData.getLength();
    }

    /* calculate mean q bordvoll (weighted by length) */
    double completeLength = 0.0;
    double meanQbordvoll = 0.0;
    for( int i = 0; i < qBordvoll.length; i++ )
    {
      completeLength += length[i];
      meanQbordvoll += qBordvoll[i] * length[i];
    }

    return meanQbordvoll / completeLength;
  }


  public ProfileData[] getAllProfiles( )
  {
    return m_profileData;
  }

  /**
   * This function returns the start position (the first station).
   * 
   * @return The start position (the first station).
   */
  public BigDecimal getStartStation( )
  {
    if( m_profileData.length == 0 )
      return null;

    return m_profileData[0].getStation();
  }

  /**
   * This function returns the end position (the last station).
   * 
   * @return The end position (the last station).
   */
  public BigDecimal getEndStation( )
  {
    if( m_profileData.length == 0 )
      return null;

    return m_profileData[m_profileData.length - 1].getStation();
  }

  public IKMValue[] getKMValues( final double fullLength, final int paramCount ) throws CoreException
  {
    final int[] qIndices = buildMapping( paramCount + 1 );
    final IKMValue[] result = new IKMValue[paramCount];
    final int middleParamIndex = getMiddleParamIndex( paramCount + 1 );
    for( int i = 0; i < paramCount; i++ )
    {
      final IKMValue mergedKM = getMergedKM( fullLength, qIndices[i], qIndices[i + 1] );
      // REMARK: as we are using a mean qbordvoll, we get nForeland and kForeland values below the qBordvollIndex.
      // We force those to 0.0;
      if( i < middleParamIndex )
        result[i] = new NoForelandKMValue( mergedKM );
      else
        result[i] = mergedKM;
    }
    return result;
  }

  /**
   * Calculates which paramCount-index maps to which q-index.
   */
  private int[] buildMapping( final int paramCount ) throws CoreException
  {
    final double qBordvoll = getQBordvoll();
    final int qBordvollIndex = findQBordvollIndex( qBordvoll );
    if( qBordvollIndex == -1 )
      throw fail( Messages.getString("ProfileDataSet_0"), qBordvoll ); //$NON-NLS-1$

    final int numberQ = getNumberQ();
    final int maxIndexQ = numberQ - 1;

    final int middleParamIndex = getMiddleParamIndex( paramCount );

    final int[] qIndices = new int[paramCount];

    if( !buildMapping( qIndices, 0, middleParamIndex, 0, qBordvollIndex ) )
      throw fail( Messages.getString("ProfileDataSet_1"), qBordvoll ); //$NON-NLS-1$

    if( !buildMapping( qIndices, middleParamIndex, paramCount, qBordvollIndex, numberQ ) )
      throw fail( Messages.getString("ProfileDataSet_2"), qBordvoll ); //$NON-NLS-1$

    qIndices[paramCount - 1] = maxIndexQ;

    return qIndices;
  }

  private CoreException fail( final String format, final double qBordvoll )
  {
    final String message = String.format( format, qBordvoll );
    final IStatus status = new Status( IStatus.ERROR, KMPlugin.getID(), message );
    return new CoreException( status );
  }

  private int getMiddleParamIndex( final int paramCount )
  {
    final int middleParamIndex = (int) ((paramCount / 2.0f));
    return middleParamIndex;
  }

  private boolean buildMapping( final int[] qIndices, final int paramFrom, final int paramTo, final int qIndexFrom, final int qIndexTo )
  {
    final double interval = ((double) qIndexTo - qIndexFrom - 1) / (paramTo - paramFrom - 1);

    if( interval < 1.0 )
      return false;

    for( int i = 0; i < paramTo - paramFrom; i++ )
      qIndices[i + paramFrom] = qIndexFrom + (int) (i * interval);
    return true;
  }

  private int getNumberQ( )
  {
    final ProfileData data = m_profileData[0];
    final int numKMValues = data.getNumberKMValues();
    return numKMValues;
  }

  /**
   * This function collects KMValues between two selected discharges over all profiles.
   * 
   * @param indexQfrom
   *          Lower index of the discharge.
   * @param indexQto
   *          Upper index of the discharge.
   * @param fullLength
   *          The full length of this calculated strand. The profile length will be adjusted so that their complete
   *          length will we of this same full length.
   * @return The km values derived between the given two discharge values.
   */
  private IKMValue getMergedKM( final double fullLength, final int indexQfrom, final int indexQto )
  {
    /* Only in this case we have one single degenerate profile with length NaN. */
    if( m_profileData.length == 1 && Double.isNaN( m_profileData[0].getLength() ) )
      return m_profileData[0].getKMValue( fullLength, indexQfrom, indexQto );

    double profileFullLength = 0;
    for( final ProfileData element : m_profileData )
      profileFullLength += element.getLength();

    final double lengthFactor = fullLength / profileFullLength;

    final IKMValue[] profileKMs = new IKMValue[m_profileData.length];
    for( int i = 0; i < profileKMs.length; i++ )
      profileKMs[i] = m_profileData[i].getKMValue( lengthFactor, indexQfrom, indexQto );
    return new MultiKMValue( profileKMs );
  }

  /**
   * Returns the index of qBordvoll. We are currently using the index of the biggest q below qBordvoll
   */
  private int findQBordvollIndex( final double qBordvoll )
  {
    return m_profileData[0].getQBordvollIndex( qBordvoll );
  }
}