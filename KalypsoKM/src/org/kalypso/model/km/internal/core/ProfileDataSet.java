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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.km.internal.KMPlugin;


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
      data.calculateLength( prevProfile, nextProfile );
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
      m_startPosition = profileData[0].getStation();
      m_endPosition = profileData[size - 1].getStation();
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

  public IKMValue[] getKMValues( final int paramCount ) throws CoreException
  {
    final double qBordvoll = findQBordvoll();

    final int[] qIndices = buildMapping( paramCount + 1, qBordvoll );
    final IKMValue[] result = new IKMValue[paramCount];
    final int middleParamIndex = getMiddleParamIndex( paramCount + 1 );
    for( int i = 0; i < paramCount; i++ )
    {
      final IKMValue mergedKM = getMergedKM( qIndices[i], qIndices[i + 1] );
      // HACKY: as we are using a mean qbordvoll, we get nForeland and kForeland values below the qBordvollIndex.
      // We force those to 0.0; TODO: check if correct.
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
  private int[] buildMapping( final int paramCount, final double qBordvoll ) throws CoreException
  {
    final int qBordvollIndex = findQBordvollIndex( qBordvoll );
    if( qBordvollIndex == -1 )
    {
      fail( "Mittleres Q-Bordvoll (%.3f) ausserhalb des berechneten Bereichs. WSPM Neubrechnnug notwendig.", qBordvoll );
      // TODO: error
      return null;
    }

    final int numberQ = getNumberQ();
    final int maxIndexQ = numberQ - 1;

    final int middleParamIndex = getMiddleParamIndex( paramCount );

    final int[] qIndices = new int[paramCount];

    if( !buildMapping( qIndices, 0, middleParamIndex, 0, qBordvollIndex + 1 ) )
      fail( "Nicht ausreichend Abflusswerte unterhalb von Q-Bordvoll (%.3f) vorhanden. WSPM Neubrechnnug mit kleinerem Q-min notwendig.", qBordvoll );

    if( !buildMapping( qIndices, middleParamIndex - 1, paramCount, qBordvollIndex, numberQ ) )
      fail( "Nicht ausreichend Abflusswerte oberhalb von Q-Bordvoll (%.3f) vorhanden. WSPM Neubrechnnug mit grˆﬂerem Q-max notwendig.", qBordvoll );

    qIndices[paramCount - 1] = maxIndexQ;

    return qIndices;
  }

  private void fail( final String format, final double qBordvoll ) throws CoreException
  {
    final String message = String.format( format, qBordvoll );
    final IStatus status = new Status( IStatus.ERROR, KMPlugin.getID(), message );
    throw new CoreException( status );
  }

  protected int getMiddleParamIndex( final int paramCount )
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
   * @return The km values derived between the given two discharge values.
   */
  private IKMValue getMergedKM( final int indexQfrom, final int indexQto )
  {
    final IKMValue[] profileKMs = new IKMValue[m_profileData.length];

    for( int i = 0; i < profileKMs.length; i++ )
      profileKMs[i] = m_profileData[i].getKMValue( indexQfrom, indexQto );

    return new MultiKMValue( profileKMs );
  }

  /**
   * Returns the index of qBordvoll. We are currently using the index of the biggest q below qBordvoll
   */
  private int findQBordvollIndex( final double qBordvoll )
  {
    return m_profileData[0].getQBordvollIndex( qBordvoll );
  }

  /**
   * This function calculates qBordvoll
   */
  private double findQBordvoll( )
  {
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
}