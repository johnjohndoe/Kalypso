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

import com.vividsolutions.jts.geom.Polygon;

/**
 * @author Gernot Belger
 */
public class RiskStatisticItem
{
  private final Collection<StatisticArea> m_areas = new ArrayList<>();

  private final SortedMap<Integer, SpecificDamageStatistic> m_specificDamagestatistics = new TreeMap<>();

  private final AverageDamageStatistic m_averageDamageStatistic = new AverageDamageStatistic();

  private final StatisticItemKey m_key;

  public RiskStatisticItem( final StatisticItemKey key )
  {
    m_key = key;
  }

  public void addSpecificDamage( final int returnPeriod, final double value, final double cellArea )
  {
    final SpecificDamageStatistic statistic = getStatisticEntry( returnPeriod );
    statistic.updateStatistic( value, cellArea );
  }

  private SpecificDamageStatistic getStatisticEntry( final int returnPeriod )
  {
    if( !m_specificDamagestatistics.containsKey( returnPeriod ) )
      m_specificDamagestatistics.put( returnPeriod, new SpecificDamageStatistic( returnPeriod ) );

    return m_specificDamagestatistics.get( returnPeriod );
  }

  /**
   * adds a average annual damage value to the polygon
   */
  public void addAverageAnnualDamage( final double value, final double cellArea )
  {
    if( m_key.getName().equals( "grassland" ) )
    {
      System.out.println();
    }

    m_averageDamageStatistic.addAverageAnnualDamage( value, cellArea );
  }

  public double getAnnualAverageDamage( )
  {
    return m_averageDamageStatistic.getAverageAnnualDamage();
  }

  // FIXME: must be the same code that calculates the risk zone values- > remove
  /**
   * calculates the average annual damage value for this item <br>
   * The value is calculated by integrating the specific damage values.<br>
   */
  public double calcAnnualAverageDamage( )
  {
    if( m_specificDamagestatistics.size() == 0 )
      return 0.0;

    final Integer[] periods = m_specificDamagestatistics.keySet().toArray( new Integer[m_specificDamagestatistics.size()] );

    /* calculate the average annual damage by integrating the specific damage values */
    double averageSum = 0.0;

    for( int i = 0; i < periods.length - 1; i++ )
    {
      /* get the probability for each return period */
      final double p1 = 1 / periods[i];
      final double p2 = 1 / periods[i + 1];

      /* calculate the difference */
      final double d_pi = p1 - p2;

      /*
       * get the specific damage summation value for this and the next return period an calculate the difference
       * (divided by 2). This means nothing else than to calculate the area for trapezoid with ha=specific value 1 and
       * hb= specific value 2. The width of the trapezoid is the difference of the probabilities that belong to both
       * specific damages values.
       */
      final SpecificDamageStatistic statEntry1 = m_specificDamagestatistics.get( periods[i] );
      final SpecificDamageStatistic statEntry2 = m_specificDamagestatistics.get( periods[i + 1] );

      // final BigDecimal sumStat = statEntry2.getDamageSum().add( statEntry1.getDamageSum() );
      final double sumStat = statEntry2.getAverageDamage() + statEntry1.getAverageDamage();
      final double si = sumStat / 2;

      /* calculate the average damage and add it */
      averageSum = averageSum + si * d_pi;
    }

    return averageSum;
  }

  public void add( final Polygon area )
  {
    m_areas.add( new StatisticArea( this, area ) );
  }

  public SpecificDamageStatistic[] getSpecificDamages( )
  {
    return m_specificDamagestatistics.values().toArray( new SpecificDamageStatistic[m_specificDamagestatistics.size()] );
  }

  @Override
  public String toString( )
  {
    final StringBuilder buffer = new StringBuilder();

    buffer.append( "Key: " ).append( m_key.getName() ).append( '\n' );
    buffer.append( "Group: " ).append( m_key.getGroupLabel() ).append( '\n' );
    buffer.append( "Specific Damages:\n" );

    for( final Entry<Integer, SpecificDamageStatistic> entry : m_specificDamagestatistics.entrySet() )
    {
      buffer.append( "\tReturn Period: " ).append( entry.getKey() + "\t" );
      buffer.append( entry.getValue() ).append( '\n' );
    }

    buffer.append( "Average Damage:\n" );
    buffer.append( m_averageDamageStatistic.toString() );

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