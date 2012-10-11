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

import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.phenomenon.Phenomenon;
import org.kalypso.observation.result.Component;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
class StatisticObservationBuilder
{
  private static final String DICT_URN = "urn:ogc:gml:dict:kalypso:risk:model:riskresultstat"; //$NON-NLS-1$

  private static final String DICT_ANNUAL = DICT_URN + "#ANNUAL"; //$NON-NLS-1$

  private static final String DICT_TOTAL = DICT_URN + "#TOTAL"; //$NON-NLS-1$

  private final IRasterizationControlModel m_controlModel;

  private final Integer[] m_returnPeriods;

  private final RiskStatisticItem[] m_items;

  private final RiskStatisticItem m_total;

  public StatisticObservationBuilder( final IRasterizationControlModel controlModel, final Integer[] returnPeriods, final RiskStatisticItem[] items, final RiskStatisticItem total )
  {
    m_controlModel = controlModel;
    m_returnPeriods = returnPeriods;
    m_items = items;
    m_total = total;
  }

  public void execute( )
  {
    /* create an observation */
    final Feature fObs = m_controlModel.createSubFeature( IRasterizationControlModel.PROPERTY_STATISTIC_OBS );

    // new observation
    final TupleResult result = new TupleResult();

    addComponentsToObs( fObs, result );

    /* fill TupleResult with data */
    fillResultWithData( result );

    /* add observation to workspace */
    final IObservation<TupleResult> obs = new Observation<>( "name", "description", result ); //$NON-NLS-1$ //$NON-NLS-2$
    // maybe set phenomenon?
    ObservationFeatureFactory.toFeature( obs, fObs );
  }

  private void addComponentsToObs( final Feature fObs, final TupleResult result )
  {
    /* add the landuse class name component */
    final String landuseHeader = "Landuse"; //$NON-NLS-1$
    final Component componentLanduse = new Component( "", landuseHeader, landuseHeader, "", "", XmlTypes.XS_STRING, "null", new Phenomenon( "", landuseHeader, landuseHeader ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    result.addComponent( componentLanduse );

    final String groupHeader = "Group"; //$NON-NLS-1$
    final Component componentGroup = new Component( groupHeader, groupHeader, groupHeader, "", "", XmlTypes.XS_STRING, "null", new Phenomenon( "", groupHeader, groupHeader ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    result.addComponent( componentGroup );

    for( final Integer returnPeriod : m_returnPeriods )
    {
      final String eventName = Messages.getString( "org.kalypso.risk.model.utils.RiskLanduseHelper.1", returnPeriod ); //$NON-NLS-1$
      //      m_description = Messages.getString( "org.kalypso.risk.model.utils.RiskLanduseHelper.2" ) + returnPeriod + Messages.getString( "org.kalypso.risk.model.utils.RiskLanduseHelper.3" ); //$NON-NLS-1$ //$NON-NLS-2$

      final String headerNameTotalDamage = "TotalDamage_" + eventName; //$NON-NLS-1$
      final String headerNameFloodedArea = "FloodedArea_" + eventName;//$NON-NLS-1$
      final String headerNameAveragedDamage = "AverageDamage_" + eventName;//$NON-NLS-1$

      final IComponent valueComponentTotalDamage = new Component( "", headerNameTotalDamage, headerNameTotalDamage, "none", "", XmlTypes.XS_DECIMAL, BigDecimal.valueOf( 0.0 ), new Phenomenon( "", "TotalDamage", headerNameTotalDamage ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
      result.addComponent( valueComponentTotalDamage );

      final IComponent valueComponentFloodedArea = new Component( "", headerNameFloodedArea, headerNameFloodedArea, "none", "", XmlTypes.XS_DECIMAL, BigDecimal.valueOf( 0.0 ), new Phenomenon( "", "FloodedArea", headerNameFloodedArea ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
      result.addComponent( valueComponentFloodedArea );

      final IComponent valueComponentAveragedDamage = new Component( "", headerNameAveragedDamage, headerNameAveragedDamage, "none", "", XmlTypes.XS_DECIMAL, BigDecimal.valueOf( 0.0 ), new Phenomenon( "", "AveragedDamage", headerNameAveragedDamage ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
      result.addComponent( valueComponentAveragedDamage );
    }

    /* add the average annual damage component */
    result.addComponent( ObservationFeatureFactory.createDictionaryComponent( fObs, DICT_TOTAL ) );
    result.addComponent( ObservationFeatureFactory.createDictionaryComponent( fObs, DICT_ANNUAL ) );
  }

  private void fillResultWithData( final TupleResult result )
  {
    for( final RiskStatisticItem item : m_items )
      addRow( result, item );

    addRow( result, m_total );
  }

  private void addRow( final TupleResult result, final RiskStatisticItem item )
  {
    /* add the data to the observation */
    final IRecord newRecord = result.createRecord();

    /* Name and Group */
    final StatisticItemKey key = item.getKey();
    newRecord.setValue( 0, key.getName() );
    newRecord.setValue( 1, key.getGroupLabel() );

    final IComponent[] components = result.getComponents();

    // specific damage values for each event
    // Add values in same order as components have been changed
    int i = 2;
    for( final Integer returnPeriod : m_returnPeriods )
    {
      final SpecificDamageStatistic specificDamamage = item.getSpecificDamage( returnPeriod );

      newRecord.setValue( i++, new BigDecimal( specificDamamage.getTotalDamageValue() ).setScale( RiskModelHelper.BIGDECIMAL_SCALE_COARSE, BigDecimal.ROUND_HALF_UP ) );
      newRecord.setValue( i++, new BigDecimal( specificDamamage.getTotalFloodedArea() ).setScale( RiskModelHelper.BIGDECIMAL_SCALE_COARSE, BigDecimal.ROUND_HALF_UP ) );
      newRecord.setValue( i++, new BigDecimal( specificDamamage.getAverageDamage() ).setScale( RiskModelHelper.BIGDECIMAL_SCALE_COARSE, BigDecimal.ROUND_HALF_UP ) );
    }

    newRecord.setValue( components.length - 2, item.calcAnnualTotalDamage() );
    newRecord.setValue( components.length - 1, item.calcAnnualAverageDamage() );

    result.add( newRecord );
  }
}