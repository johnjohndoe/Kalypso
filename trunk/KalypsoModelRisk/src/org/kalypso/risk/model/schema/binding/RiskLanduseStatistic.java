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
package org.kalypso.risk.model.schema.binding;

import java.math.BigDecimal;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Thomas Jung
 * 
 */
public class RiskLanduseStatistic extends Feature_Impl implements IRiskLanduseStatistic, Comparable<IRiskLanduseStatistic>
{

  public RiskLanduseStatistic( Object parent, IRelationType parentRelation, IFeatureType ft, String id, Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  private int m_numberOfEntries = 0;

  // minimal occurred cell value
  private BigDecimal m_min = new BigDecimal( Double.MAX_VALUE ).setScale( 2, BigDecimal.ROUND_HALF_UP );

  // maximal occured cell value
  private BigDecimal m_max = new BigDecimal( -Double.MAX_VALUE ).setScale( 2, BigDecimal.ROUND_HALF_UP );

  // average value of all cells
  private BigDecimal m_average = new BigDecimal( 0 ).setScale( 2, BigDecimal.ROUND_HALF_UP );

  // summation of all values
  private BigDecimal m_sum = new BigDecimal( 0 ).setScale( 2, BigDecimal.ROUND_HALF_UP );

  private BigDecimal m_cellSize = null;


  /**
   * @see org.kalypso.risk.model.schema.binding.IRiskLanduseStatistic#getAverageDamage()
   */
  @Override
  public BigDecimal getAverageDamage( )
  {
    return (BigDecimal) getProperty( IRiskLanduseStatistic.PROPERTY_AVERAGE_DAMAGE );
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.IRiskLanduseStatistic#getDamageSum()
   */
  @Override
  public BigDecimal getDamageSum( )
  {
    return (BigDecimal) getProperty( IRiskLanduseStatistic.PROPERTY_DAMAGE_SUM );
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.IRiskLanduseStatistic#getMaxDamage()
   */
  @Override
  public BigDecimal getMaxDamage( )
  {
    return (BigDecimal) getProperty( IRiskLanduseStatistic.PROPERTY_MAX_DAMAGE );
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.IRiskLanduseStatistic#getMinDamage()
   */
  @Override
  public BigDecimal getMinDamage( )
  {
    return (BigDecimal) getProperty( IRiskLanduseStatistic.PROPERTY_MIN_DAMAGE );
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.IRiskLanduseStatistic#getReturnPeriod()
   */
  @Override
  public int getReturnPeriod( )
  {
    final Integer value = (Integer) getProperty( IRiskLanduseStatistic.PROPERTY_RETURN_PERIOD );
    return value == null ? 0 : value.intValue();
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.IRiskLanduseStatistic#updateStatistic(java.math.BigDecimal)
   */
  @Override
  public void updateStatistic( final BigDecimal value )
  {
    m_numberOfEntries++;

    m_min = m_min.min( value );
    m_max = m_max.max( value );

    m_sum = m_sum.add( value );
    final double averageValue = (m_sum.doubleValue() / m_numberOfEntries);
    m_average = new BigDecimal( averageValue ).setScale( 2, BigDecimal.ROUND_HALF_UP );

    update();
  }

  public void update( )
  {
    setProperty( IRiskLanduseStatistic.PROPERTY_MIN_DAMAGE, m_min );
    setProperty( IRiskLanduseStatistic.PROPERTY_MAX_DAMAGE, m_max );
    setProperty( IRiskLanduseStatistic.PROPERTY_AVERAGE_DAMAGE, m_average );
    setProperty( IRiskLanduseStatistic.PROPERTY_DAMAGE_SUM, m_sum );
  }

  @Override
  public void finish( )
  {
    /* calculate the landuse area [m²] */
    final double totalArea = m_cellSize.doubleValue() * m_numberOfEntries;
    final BigDecimal totalAreaValue = new BigDecimal( totalArea ).setScale( 2, BigDecimal.ROUND_HALF_UP );
    setProperty( IRiskLanduseStatistic.PROPERTY_FLOODED_AREA, totalAreaValue );

    /* calculate the total damage [€] */
    final BigDecimal totalDamage = m_average.multiply( totalAreaValue );
    setProperty( IRiskLanduseStatistic.PROPERTY_TOTAL_DAMAGE, totalDamage );
  }

  @Override
  public void setCellSize( final BigDecimal cellSize )
  {
    m_cellSize = cellSize;
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.IRiskLanduseStatistic#setReturnPeriod(int)
   */
  @Override
  public void setReturnPeriod( final int returnPeriod )
  {
    setProperty( IRiskLanduseStatistic.PROPERTY_RETURN_PERIOD, returnPeriod );
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.IRiskLanduseStatistic#getTotalDamage()
   */
  @Override
  public BigDecimal getTotalDamage( )
  {
    return (BigDecimal) getProperty( IRiskLanduseStatistic.PROPERTY_TOTAL_DAMAGE );
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.IRiskLanduseStatistic#getAverageAnnualDamage()
   */
  @Override
  public BigDecimal getAverageAnnualDamage( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.IRiskLanduseStatistic#setTotalDamage(java.math.BigDecimal)
   */
  @Override
  public void setTotalDamage( final BigDecimal averageAnnualDamage )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo( final IRiskLanduseStatistic o )
  {
    return Integer.valueOf( getReturnPeriod() ).compareTo( Integer.valueOf( o.getReturnPeriod() ) );
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.IRiskLanduseStatistic#getFloodedArea()
   */
  @Override
  public BigDecimal getFloodedArea( )
  {
    return (BigDecimal) getProperty( IRiskLanduseStatistic.PROPERTY_FLOODED_AREA );
  }

}
