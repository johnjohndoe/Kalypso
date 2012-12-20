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
package org.kalypso.risk.model.simulation.statistics;

import org.apache.commons.lang3.builder.ToStringBuilder;

/**
 * @author Gernot Belger
 */
public class SpecificDamageStatistic
{
  private final int m_returnPeriod;

  private double m_min = Double.POSITIVE_INFINITY;

  private double m_max = Double.NEGATIVE_INFINITY;

  private double m_sum = 0;

  private double m_totalArea = 0;

  public SpecificDamageStatistic( final int returnPeriod )
  {
    m_returnPeriod = returnPeriod;
  }

  public void updateStatistic( final double value, final double cellArea )
  {
    m_min = Math.min( m_min, value );
    m_max = Math.max( m_max, value );

    m_sum += value * cellArea;
    m_totalArea += cellArea;
  }

  public double getTotalFloodedArea( )
  {
    return m_totalArea;
  }

  public double getTotalDamageValue( )
  {
    return m_sum;

    // TODO: old, should be the same
    // return getAverage() * getTotalFloodedArea();
  }

  public double getAverageDamage( )
  {
    final double averageDamage = m_sum / m_totalArea;
    if( Double.isNaN( averageDamage ) || Double.isInfinite( averageDamage ) )
      return 0.0;

    return averageDamage;
  }

  public int getReturnPeriod( )
  {
    return m_returnPeriod;
  }

  @Override
  public String toString( )
  {
    return ToStringBuilder.reflectionToString( this );
  }
}