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
package org.kalypso.model.hydrology.timeseries;

import java.util.Date;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.commons.time.PeriodUtils;
import org.kalypso.contribs.java.util.CalendarUtilities.FIELD;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.metadata.MetadataHelper;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.timeseries.base.CacheTimeSeriesVisitor;
import org.kalypso.ogc.sensor.util.DataSetTupleModelBuilder;
import org.kalypso.ogc.sensor.util.Observations;

/**
 * Apply some base meta data values for KalypsoHydrology.
 *
 * @author Gernot Belger
 */
public class HydrologyTimeseriesImportWorker
{
  private final IObservation m_observation;

  private final DateRange m_daterange;

  public HydrologyTimeseriesImportWorker( final IObservation observation, final DateRange daterange )
  {
    m_observation = observation;
    m_daterange = daterange;
  }

  public IObservation convert( final Period period, final LocalTime timestamp ) throws CoreException
  {
    try
    {
      final IAxis[] axes = m_observation.getAxes();
      final IAxis valueAxis = AxisUtils.findValueAxis( axes, true );

      /**
       * getValues() will change the metadata of the observation filter. so call it first. <br>
       * date range is needed by interval filter
       */
      final DateRange daterange = getDateRange();
      final ITupleModel base = m_observation.getValues( new ObservationRequest( daterange ) );
      final MetadataList metadata = MetadataHelper.clone( m_observation.getMetadataList() );
      final CacheTimeSeriesVisitor visitor = new CacheTimeSeriesVisitor( metadata );
      base.accept( visitor, 1 );

      final DataSetTupleModelBuilder builder = new DataSetTupleModelBuilder( metadata, visitor.getValueMap() );
      builder.execute( new NullProgressMonitor() );

      final ITupleModel model = builder.getModel();

      final SimpleObservation observation = new SimpleObservation( m_observation.getHref(), m_observation.getName(), metadata, model );
      final IObservation resultObservation = removeMissingValues( observation, valueAxis );
      doUpdateMetadata( metadata, period, timestamp, daterange );

// final TIMESERIES_TYPE type = TimeseriesUtils.getType( valueAxis.getType() );
// if( TIMESERIES_TYPE.eInstantaneousValue.equals( type ) )
// {
// final ZmlInterpolationWorker interpolation = new ZmlInterpolationWorker( resultObservation.getValues( null ),
// resultObservation.getMetadataList(), valueAxis );
// interpolation.execute( new NullProgressMonitor() );
// }

      return resultObservation;
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, "Failed to clean timeseries" ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }

  private void doUpdateMetadata( final MetadataList metadata, final Period period, final LocalTime timestamp, final DateRange daterange )
  {
    /* Set timestep. */
    if( period != null )
    {
      final int amount = PeriodUtils.findCalendarAmount( period );
      final FIELD field = PeriodUtils.findCalendarField( period );
      MetadataHelper.setTimestep( metadata, field.getField(), amount );
    }

    /* Set timestamp. */
    if( timestamp != null )
      MetadataHelper.setTimestamp( metadata, timestamp );

    MetadataHelper.setTargetDateRange( metadata, daterange );
  }

  private DateRange getDateRange( )
  {
    if( m_daterange == null )
      return null;

    final Date from = m_daterange.getFrom();
    final Date to = m_daterange.getTo();
    if( Objects.allNotNull( from, to ) )
      return new DateRange( from, to );

    return Observations.findDateRange( m_observation );
  }

  private IObservation removeMissingValues( final IObservation observation, final IAxis valueAxis ) throws SensorException
  {
    final String parameterType = valueAxis.getType();

    switch( parameterType )
    {
      case ITimeseriesConstants.TYPE_RUNOFF:
      case ITimeseriesConstants.TYPE_WATERLEVEL:
        // ignore W and Q for now, old mechanism
        return observation;

        // FIXME: add new constants here
      case ITimeseriesConstants.TYPE_RAINFALL:
      case ITimeseriesConstants.TYPE_EVAPORATION:
        observation.accept( new SetMissingValuesTo0Visitor( 0.0, Double.NaN ), null, 1 );
        return observation;

      case ITimeseriesConstants.TYPE_TEMPERATURE:
        // FIXME: is 0 good for temperature?! maybe not necessary, because later we will interpolate missing values
        observation.accept( new SetMissingValuesTo0Visitor( -273.15, 99 ), null, 1 );
        return observation;

      default:
        return observation;
    }
  }
}