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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map.Entry;
import java.util.SortedMap;
import java.util.TreeMap;

import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.utils.RiskModelHelper;

import com.vividsolutions.jts.geom.Polygon;

/**
 * @author Gernot Belger
 */
public class RiskStatisticItem
{
  private final Collection<StatisticArea> m_areas = new ArrayList<>();

  private final SortedMap<Integer, SpecificDamageStatistic> m_specificDamagestatistics = new TreeMap<>();

  private final StatisticItemKey m_key;

  public RiskStatisticItem( final StatisticItemKey key )
  {
    m_key = key;
  }

  public void addSpecificDamage( final int returnPeriod, final double value, final double cellArea )
  {
    final SpecificDamageStatistic statistic = getSpecificDamage( returnPeriod );
    statistic.updateStatistic( value, cellArea );
  }

  public SpecificDamageStatistic getSpecificDamage( final int returnPeriod )
  {
    if( !m_specificDamagestatistics.containsKey( returnPeriod ) )
      m_specificDamagestatistics.put( returnPeriod, new SpecificDamageStatistic( returnPeriod ) );

    return m_specificDamagestatistics.get( returnPeriod );
  }

  /**
   * calculates the average annual damage value for this item <br>
   * The value is calculated by integrating the specific damage values.<br>
   */
  public double calcAnnualTotalDamage( )
  {
    if( m_specificDamagestatistics.size() == 0 )
      return 0.0;

    final Integer[] periods = m_specificDamagestatistics.keySet().toArray( new Integer[m_specificDamagestatistics.size()] );

    final double[] probabilities = new double[periods.length];
    for( int i = 0; i < probabilities.length; i++ )
      probabilities[i] = 1.0 / periods[i];

    final double[] totalDamages = new double[periods.length];
    for( int i = 0; i < totalDamages.length; i++ )
      totalDamages[i] = m_specificDamagestatistics.get( periods[i] ).getTotalDamageValue();

    return RiskModelHelper.calcPotentialAnnualDamageValue( totalDamages, probabilities );
  }

  public double calcAnnualAverageDamage( )
  {
    final double totalDamage = calcAnnualTotalDamage();

    final double floodedArea = calculateMaximalFloodedArea();

    if( Double.isNaN( floodedArea ) || floodedArea == 0.0 )
      return 0.0;

    return totalDamage / floodedArea;
  }

  private double calculateMaximalFloodedArea( )
  {
    double floodedArea = 0.0;

    final Collection<SpecificDamageStatistic> values = m_specificDamagestatistics.values();
    for( final SpecificDamageStatistic specificDamageStatistic : values )
    {
      final double specificFloodedArea = specificDamageStatistic.getTotalFloodedArea();
      floodedArea = Math.max( floodedArea, specificFloodedArea );
    }

    return floodedArea;
  }

  public void add( final Polygon area )
  {
    m_areas.add( new StatisticArea( this, area ) );
  }

  @Override
  public String toString( )
  {
    final StringBuilder buffer = new StringBuilder();

    buffer.append( Messages.getString( "RiskStatisticItem_0" ) ).append( m_key.getName() ).append( '\n' ); //$NON-NLS-1$
    buffer.append( Messages.getString( "RiskStatisticItem_1" ) ).append( m_key.getGroupLabel() ).append( '\n' ); //$NON-NLS-1$
    buffer.append( Messages.getString( "RiskStatisticItem_2" ) ); //$NON-NLS-1$

    for( final Entry<Integer, SpecificDamageStatistic> entry : m_specificDamagestatistics.entrySet() )
    {
      buffer.append( Messages.getString( "RiskStatisticItem_3" ) ).append( entry.getKey() + "\t" ); //$NON-NLS-1$ //$NON-NLS-2$
      buffer.append( entry.getValue() ).append( '\n' );
    }

    buffer.append( Messages.getString( "RiskStatisticItem_5" ) ); //$NON-NLS-1$

    return buffer.toString();
  }

  public StatisticArea[] getAreas( )
  {
    return m_areas.toArray( new StatisticArea[m_areas.size()] );
  }

  public StatisticItemKey getKey( )
  {
    return m_key;
  }
}