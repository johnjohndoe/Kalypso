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
import java.util.Set;
import java.util.TreeSet;

import org.kalypso.commons.xml.XmlTypes;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.phenomenon.Phenomenon;
import org.kalypso.observation.result.Component;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.FeatureComponent;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypsodeegree.model.feature.Feature;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.index.ItemVisitor;
import com.vividsolutions.jts.index.SpatialIndex;
import com.vividsolutions.jts.index.quadtree.Quadtree;

/**
 * @author Gernot Belger
 */
public class StatisticCollector
{
  private static final String DICT_URN = "urn:ogc:gml:dict:kalypso:risk:model:riskresultstat"; //$NON-NLS-1$

  private static final String DICT_ANNUAL = DICT_URN + "#ANNUAL"; //$NON-NLS-1$

  private final Set<Integer> m_returnPeriods = new TreeSet<>();

  private SpatialIndex m_elements;

  private Envelope m_fullExtent;

  public void addSpecificDamage( final int returnPeriod, final Coordinate position, final double cellArea )
  {
    m_returnPeriods.add( returnPeriod );

    final SpecificDamageVisitor visitor = new SpecificDamageVisitor( returnPeriod, position, cellArea );

    final Envelope searchEnv = new Envelope( position );
    m_elements.query( searchEnv, visitor );
  }

  public void addAverageDamage( final Coordinate position, final double cellArea )
  {
    final AverageDamageVisitor visitor = new AverageDamageVisitor( position, cellArea );

    final Envelope searchEnv = new Envelope( position );
    m_elements.query( searchEnv, visitor );
  }

  public void setItems( final RiskStatisticItem[] items )
  {
    final Envelope fullExtent = new Envelope();

    final Quadtree quadtree = new Quadtree();

    for( final RiskStatisticItem item : items )
    {
      final Envelope envelope = item.getArea().getEnvelopeInternal();
      fullExtent.expandToInclude( envelope );
      quadtree.insert( envelope, item );
    }

    m_fullExtent = fullExtent;
    m_elements = quadtree;
    System.out.println( String.format( "Depth: %d", quadtree.depth() ) );
  }

  public void createResultObservation( final IRasterizationControlModel controlModel )
  {
    // TODO: merge elements of same kind

    // final List<ILanduseClass> landuseClassesList = controlModel.getLanduseClassesList();

    /* create an observation */
    final Feature fObs = controlModel.createSubFeature( IRasterizationControlModel.PROPERTY_STATISTIC_OBS );

    // new observation
    final TupleResult result = new TupleResult();

    addComponentsToObs( fObs, result );

    /* fill TupleResult with data */
    fillResultWithData( result );

    /* add observation to workspace */
    final IObservation<TupleResult> obs = new Observation<TupleResult>( "name", "description", result ); //$NON-NLS-1$ //$NON-NLS-2$
    // maybe set phenomenon?
    ObservationFeatureFactory.toFeature( obs, fObs );
  }

  private void addComponentsToObs( final Feature fObs, final TupleResult result )
  {
    /* add the landuse class name component */
    final String landuseHeader = "Landuse"; //$NON-NLS-1$

    final Component componentLanduse = new Component( "", landuseHeader, landuseHeader, "", "", XmlTypes.XS_STRING, "null", new Phenomenon( "", landuseHeader, landuseHeader ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    result.addComponent( componentLanduse );

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
    final FeatureComponent createDictionaryComponent = ObservationFeatureFactory.createDictionaryComponent( fObs, DICT_ANNUAL );

    result.addComponent( createDictionaryComponent );
  }

  private void fillResultWithData( final TupleResult result )
  {
    /* get the size of the table */
    // number of columns is the one for the landuse names + number of flood events + last column
    /* fill for every landuse class */
    // final ILanduseClass[] classes = RiskLanduseHelper.getLanduseTypeSet( controlModel, vectorModel );

    final ItemVisitor visitor = new FillStatisticObserationVisitor( result );
    m_elements.query( m_fullExtent, visitor );

    // FIXME
    // calculateLastRow( result );
  }

  // FIXME:
  // /**
  // * the last row of the obs table represents the following data:<br>
  // * <ul>
  // * <li>for each event:
  // * <ul>
  // * <li>the summation of all flooded areas over all landuse classes
  // * <li>the summation of all damages of over landuse classes
  // * <li>the average damage value derived by the summation values
  // * </ul>
  // * <li>the average annual damage value derived by the average damage values for each event as computed before.
  // * </ul>
  // */
  // private static void calculateLastRow( final TupleResult result )
  // {
  // /* specific damage value for each event */
  // final IRecord lastRecord = result.createRecord();
  // final int columnSize = result.getComponents().length;
  //
  // /* name */
  //    lastRecord.setValue( 0, "Total" ); //$NON-NLS-1$
  //
  // final Map<String, RiskStatisticTableValues> eventMap = new HashMap<String, RiskStatisticTableValues>();
  //
  // /* specific damage values per event */
  // // At first we fill in the summation of total damage and flooded area to be sure that these values are present if
  // we
  // // want to calculate the averaged values
  // for( int index = 1; index < columnSize; index++ )
  // {
  // final IComponent rowComp = result.getComponent( index );
  // final String compName = rowComp.getName();
  //      final String[] split = compName.split( "_" );//$NON-NLS-1$
  // final String eventType = split[0];
  // if( split.length > 1 )
  // {
  // final String eventName = split[1];
  //
  // RiskStatisticTableValues statisticTableValues = eventMap.get( eventName );
  // if( statisticTableValues == null )
  // {
  // statisticTableValues = new RiskStatisticTableValues( eventName );
  // eventMap.put( eventName, statisticTableValues );
  // }
  //
  //        if( eventType.equals( "TotalDamage" ) )//$NON-NLS-1$
  // {
  // // calculate the sum for total damage and the flooded area
  // final BigDecimal sum = calculateColumnSum( result, index );
  //
  // // set the value
  // lastRecord.setValue( index, sum );
  //
  // // remember the value
  // statisticTableValues.setTotalDamageValue( sum );
  // }
  //        else if( eventType.equals( "FloodedArea" ) )//$NON-NLS-1$
  // {
  // // calculate the sum for total damage and the flooded area
  // final BigDecimal sum = calculateColumnSum( result, index );
  //
  // // set the value
  // lastRecord.setValue( index, sum );
  //
  // // remember the value
  // statisticTableValues.setFloodedAreaValue( sum );
  // }
  // }
  // }
  //
  // // next we fill in the average damage
  // for( int index = 1; index < columnSize; index++ )
  // {
  // final IComponent rowComp = result.getComponent( index );
  // final String compName = rowComp.getName();
  //      final String[] split = compName.split( "_" );//$NON-NLS-1$
  // final String eventType = split[0];
  // if( split.length > 1 )
  // {
  // final String eventName = split[1];
  //
  // final RiskStatisticTableValues statisticTableValues = eventMap.get( eventName );
  // if( statisticTableValues == null )
  //          System.out.println( org.kalypso.risk.i18n.Messages.getString( "org.kalypso.risk.model.simulation.SimulationKalypsoRisk_RiskZonesCalculation.18" ) ); //$NON-NLS-1$
  //
  //        if( eventType.equals( "AverageDamage" ) )//$NON-NLS-1$
  // {
  // // get the values of the summation and calculate the new average value with it
  // final BigDecimal averageDamageValue = statisticTableValues.getAverageDamageValue();
  // lastRecord.setValue( index, averageDamageValue );
  // }
  // }
  // }
  //
  // // at last we calculate the average annual damage value
  // final int index = columnSize - 1;
  //
  // final IComponent rowComp = result.getComponent( index );
  // final String phenName = rowComp.getPhenomenon().getName();
  //
  //    if( phenName.equals( "AnnualValue" ) )//$NON-NLS-1$
  // {
  // // get the calculated total values for all events with corresponding periods
  // final Map<Double, RiskStatisticTableValues> periodSortedMap = getPeriods( columnSize, result, eventMap );
  //
  // /* calculate the average annual damage by integrating the specific average damage values */
  // final double averageSum = calculateAverageDamageValue( periodSortedMap );
  //
  // // calculate the sum for average annual damage
  // lastRecord.setValue( index, averageSum );
  // }
  // // and add the record
  // result.add( lastRecord );
  // }

}