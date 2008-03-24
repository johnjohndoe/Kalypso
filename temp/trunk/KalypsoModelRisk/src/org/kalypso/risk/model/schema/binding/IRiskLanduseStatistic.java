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

import javax.xml.namespace.QName;

import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * Interface for statistical data of a landuse class
 * 
 * @author Thomas Jung
 * 
 */
public interface IRiskLanduseStatistic extends IFeatureWrapper2
{
  public QName QNAME = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "StatisticClass" );

  public QName PROPERTY_RETURN_PERIOD = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "statReturnPeriod" );

  public QName PROPERTY_MIN_DAMAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "statMinDamage" );

  public QName PROPERTY_MAX_DAMAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "statMaxDamage" );

  public QName PROPERTY_AVERAGE_DAMAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "statAverageDamage" );

  public QName PROPERTY_AVERAGE_ANNUAL_DAMAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "statAverageAnnualDamage" );

  public QName PROPERTY_DAMAGE_SUM = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "statSumDamage" );

  public QName PROPERTY_TOTAL_DAMAGE = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "statTotalDamage" );

  public void updateStatistic( final BigDecimal value );

  public int getReturnPeriod( );

  public BigDecimal getMinDamage( );

  public BigDecimal getMaxDamage( );

  public BigDecimal getAverageDamage( );

  public BigDecimal getDamageSum( );

  public BigDecimal getTotalDamage( );

  public BigDecimal getAverageAnnualDamage( );

  public void setTotalDamage( final BigDecimal averageAnnualDamage );

  public void setReturnPeriod( final int returnPeriod );

  public void setCellSize( final BigDecimal cellSize );

  public void finish( );

}
