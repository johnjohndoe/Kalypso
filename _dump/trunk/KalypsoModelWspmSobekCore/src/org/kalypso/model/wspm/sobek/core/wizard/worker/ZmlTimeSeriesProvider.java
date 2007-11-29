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
package org.kalypso.model.wspm.sobek.core.wizard.worker;

import java.util.ArrayList;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.NotImplementedException;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode.BOUNDARY_TYPE;
import org.kalypso.model.wspm.sobek.core.wizard.pages.IBoundaryConditionGeneral;
import org.kalypso.model.wspm.sobek.core.wizard.pages.PageEditBoundaryConditionTimeSeries;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.template.PlainObsProvider;
import org.kalypso.ogc.sensor.zml.repository.ZmlObservationItem;
import org.kalypso.zml.obslink.TimeseriesLinkType;

import com.sun.org.apache.xerces.internal.jaxp.datatype.XMLGregorianCalendarImpl;

/**
 * @author kuch
 */
public class ZmlTimeSeriesProvider extends AbstractTimeSeriesProvider
{

  private ZmlObservationItem m_item;

  public ZmlTimeSeriesProvider( final IBoundaryConditionGeneral settings, final PageEditBoundaryConditionTimeSeries pageTS )
  {
    super( settings, pageTS );
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.wizard.worker.ITimeSeriesProvider#getBasicChanges()
   */
  @Override
  public Map<QName, Object> getBasicChanges( )
  {
    final Map<QName, Object> changes = super.getBasicChanges();

    /* set time series link */
    final TimeseriesLinkType lnkTimeSeries = new TimeseriesLinkType();

    m_item = getPageTS().getZmlObservationItem();
    final String identifier = m_item.getIdentifier();
    lnkTimeSeries.setHref( identifier );

    changes.put( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_LNK_TIME_SERIES, lnkTimeSeries );

    /* type */
    changes.put( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_CONDITION_TYPE, IBoundaryNodeLastfallCondition.BOUNDARY_CONDITION_TYPE.eZml.toGmlString() );

    return changes;
  }

  /**
   * @see org.kalypso.model.wspm.sobek.core.wizard.worker.ITimeSeriesProvider#fillTupleResult(org.kalypso.observation.result.TupleResult)
   */
  public void fillTupleResult( final TupleResult result )
  {
    final GregorianCalendar cStart = getStartDate();
    final GregorianCalendar cEnd = getEndDate();

    try
    {
      /* read zml observation - not destination result obs!!!! */
      final Object adapter = m_item.getAdapter( IObservation.class );
      if( !(adapter instanceof IObservation) )
        throw new IllegalStateException( "Invalid ZML Timeseries." );

      final IObservation observation = (IObservation) adapter;

      // TODO dateRange - zml have values before and after condition range - take these values for filling start and end
      // "spaces"/segments of observation
      final DateRange dateRange = new DateRange( cStart.getTime(), cEnd.getTime() );
      final PlainObsProvider provider = new PlainObsProvider( observation, new ObservationRequest( dateRange ) );

      final ITuppleModel values = observation.getValues( provider.getArguments() );
      if( values.getCount() == 0 )
        throw new IllegalStateException( "No Timeseries value in specified date range found!" );

      final IAxis dateAxis = getDateAxis( values.getAxisList() );
      final IAxis[] valueAxis = getValueAxis( getBoundaryNodeType(), values.getAxisList() );

      final List<Double> myValues = new ArrayList<Double>();

      /* get first segment -> boundary condition start value -> timeseries start value */
      for( final IAxis axis : valueAxis )
        myValues.add( (Double) values.getElement( 0, axis ) );

      addResult( result, cStart.getTime(), myValues );
      myValues.clear();

      /* get time series values, iterate over the whole results, start and end values are only needed for filling */
      for( int i = 0; i < values.getCount(); i++ )
      {
        final Date date = (Date) values.getElement( i, dateAxis );
        for( final IAxis axis : valueAxis )
          myValues.add( (Double) values.getElement( i, axis ) );

        addResult( result, date, myValues );
        myValues.clear();
      }
      /* get last segment -> boundary condition end value */
      for( final IAxis axis : valueAxis )
        myValues.add( (Double) values.getElement( values.getCount() - 1, axis ) );

      addResult( result, cEnd.getTime(), myValues );
      myValues.clear();
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
    }
  }

  @SuppressWarnings("deprecation")
  private void addResult( final TupleResult result, final Date date, final List<Double> myValues )
  {
    /* if wq-relation -> components must have the order date, w, q otherwise -> date, w or q */
    final IComponent[] components = result.getComponents();

    final GregorianCalendar calendar = new GregorianCalendar( date.getYear(), date.getMonth(), date.getDay(), date.getHours(), date.getMinutes(), date.getSeconds() );

    final IRecord record = result.createRecord();
    record.setValue( components[0], new XMLGregorianCalendarImpl( calendar ) );

    for( int i = 0; i < myValues.size(); i++ )
      record.setValue( components[i + 1], myValues.get( i ) );

    result.add( record );
  }

  /**
   * @return is boundary type == wq-relation {IAxis[0] = W, IAxis[1] = Q}
   */
  private IAxis[] getValueAxis( final BOUNDARY_TYPE boundary_type, final IAxis[] axisList )
  {
    String[] compare;

    if( BOUNDARY_TYPE.eW.equals( boundary_type ) )
      compare = new String[] { boundary_type.toZmlString() };
    else if( BOUNDARY_TYPE.eQ.equals( boundary_type ) )
      compare = new String[] { boundary_type.toZmlString() };
    else if( BOUNDARY_TYPE.eWQ.equals( boundary_type ) )
      compare = new String[] { BOUNDARY_TYPE.eW.toZmlString(), BOUNDARY_TYPE.eQ.toZmlString() };
    else
      throw new NotImplementedException();

    final IAxis[] myAxis = new IAxis[2];

    for( final IAxis axis : axisList )
    {
      final String type = axis.getType();
      if( type == null || "".equals( type.trim() ) )
        continue;

      if( ArrayUtils.contains( compare, type ) )
        if( !BOUNDARY_TYPE.eWQ.equals( boundary_type ) )
          return new IAxis[] { axis };
        // $ANALYSIS-IGNORE
        else if( BOUNDARY_TYPE.eW.toZmlString().equals( type ) )
          myAxis[0] = axis;
        else if( BOUNDARY_TYPE.eQ.toZmlString().equals( type ) )
          myAxis[1] = axis;
    }

    for( final IAxis a : myAxis )
      if( a == null )
        throw new IllegalStateException( "Time series value axis not found." );

    return myAxis;
  }

  private IAxis getDateAxis( final IAxis[] axisList )
  {
    for( final IAxis axis : axisList )
      if( Date.class.equals( axis.getDataClass() ) )
        return axis;

    throw new IllegalStateException( "No time series date axis defined!" );
  }
}
