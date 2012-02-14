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

import java.math.BigDecimal;

import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import com.vividsolutions.jts.index.ItemVisitor;

/**
 * @author Gernot Belger
 */
public class FillStatisticObserationVisitor implements ItemVisitor
{
  private final TupleResult m_result;

  public FillStatisticObserationVisitor( final TupleResult result )
  {
    m_result = result;
  }

  @Override
  public void visitItem( final Object item )
  {
    final RiskStatisticItem element = (RiskStatisticItem) item;

    /* add the data to the observation */
    final IRecord newRecord = m_result.createRecord();

    // landuse class name
    final String landuseName = element.getName();
    newRecord.setValue( 0, landuseName );

    final IComponent[] components = m_result.getComponents();

    // specific damage values for each event
    final SpecificDamageStatistic[] specificDamamages = element.getSpecificDamages();
    // Add values in same order as components have been changed
    int i = 1;
    for( final SpecificDamageStatistic statistic : specificDamamages )
    {
      newRecord.setValue( i++, new BigDecimal( statistic.getTotalDamageValue() ).setScale( 2, BigDecimal.ROUND_HALF_UP ) );
      newRecord.setValue( i++, new BigDecimal( statistic.getTotalFloodedArea() ).setScale( 2, BigDecimal.ROUND_HALF_UP ) );
      newRecord.setValue( i++, new BigDecimal( statistic.getAverageDamage() ).setScale( 2, BigDecimal.ROUND_HALF_UP ) );
    }

    // newRecord.setValue( components.length - 1, new BigDecimal( element.calcAnnualAverageDamage() ).setScale( 2,
    // BigDecimal.ROUND_HALF_UP ) );
    newRecord.setValue( components.length - 1, element.calcAnnualAverageDamage() );

    // final int recordSize = newRecord.getOwner().getComponents().length;
    // for( int i = 1; i < recordSize - 1; i++ )
    // {
    // final Object value = newRecord.getValue( i );
    // if( value == null )
    // newRecord.setValue( i, new BigDecimal( 0.0 ).setScale( 2, BigDecimal.ROUND_HALF_UP ) );
    // }

    // average annual damage value for the whole landuse class

    m_result.add( newRecord );
  }
}