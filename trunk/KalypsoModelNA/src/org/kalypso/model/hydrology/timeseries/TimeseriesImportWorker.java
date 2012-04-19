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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.kalypso.commons.time.PeriodUtils;
import org.kalypso.contribs.java.util.CalendarUtilities.FIELD;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.TIMESERIES_TYPE;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.metadata.MetadataHelper;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.ogc.sensor.timeseries.base.CacheTimeSeriesVisitor;
import org.kalypso.ogc.sensor.util.DataSetTupleModelBuilder;
import org.kalypso.zml.core.table.model.interpolation.ZmlInterpolationWorker;

/**
 * Helper class that should be used for every timeseries that get imported into an RRM project. Cleans the timeseries
 * from old '-999' values and add source and status axes.
 * 
 * @author Gernot Belger
 */
public class TimeseriesImportWorker
{
  private final IObservation m_observation;

  public TimeseriesImportWorker( final IObservation observation )
  {
    m_observation = observation;
  }

  public IObservation convert( final Period period, final LocalTime timestamp ) throws CoreException
  {
    try
    {
      final IAxis[] axes = m_observation.getAxes();
      final IAxis valueAxis = AxisUtils.findValueAxis( axes, true );

      final MetadataList metadata = MetadataHelper.clone( m_observation.getMetadataList() );
      final CacheTimeSeriesVisitor visitor = new CacheTimeSeriesVisitor( metadata );
      m_observation.getValues( null ).accept( visitor, 1 );

      final DataSetTupleModelBuilder builder = new DataSetTupleModelBuilder( metadata, visitor.getValueMap() );
      builder.execute( new NullProgressMonitor() );

      final ITupleModel model = builder.getModel();

      final SimpleObservation observation = new SimpleObservation( m_observation.getHref(), m_observation.getName(), metadata, model );

      final IObservation resultObservation = removeMissingValues( observation, valueAxis );

      final TIMESERIES_TYPE type = TimeseriesUtils.getType( valueAxis.getType() );
      if( TIMESERIES_TYPE.eInstantaneousValue.equals( type ) )
      {
        final ZmlInterpolationWorker interpolation = new ZmlInterpolationWorker( resultObservation.getValues( null ), resultObservation.getMetadataList(), valueAxis );
        interpolation.execute( new NullProgressMonitor() );
      }

      /* Set timestep. */
      if( period != null )
      {
        final int amount = PeriodUtils.findCalendarAmount( period );
        final FIELD field = PeriodUtils.findCalendarField( period );
        MetadataHelper.setTimestep( resultObservation.getMetadataList(), field.getField(), amount );
      }

      /* Set timestamp. */
      if( timestamp != null )
        MetadataHelper.setTimestamp( resultObservation.getMetadataList(), timestamp );

      return resultObservation;
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, "Failed to clean timeseries" );
      throw new CoreException( status );
    }
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
        observation.accept( new SetMissingValuesTo0Visior(), null, 1 );
        return observation;

      case ITimeseriesConstants.TYPE_TEMPERATURE:
        // FIXME: is 0 good for temperature?! maybe not necessary, because later we will inteprolate missing values
        observation.accept( new SetMissingValuesTo0Visior(), null, 1 );
        return observation;

      default:
        return observation;
    }
  }
}