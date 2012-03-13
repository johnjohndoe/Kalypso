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

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTupleModel;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.metadata.MetadataHelper;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.timeseries.datasource.DataSourceHandler;
import org.kalypso.ogc.sensor.visitor.IObservationValueContainer;
import org.kalypso.ogc.sensor.visitor.IObservationVisitor;
import org.kalypso.repository.IDataSourceItem;

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

  public IObservation convert( ) throws CoreException
  {
    try
    {
      final IObservation resultObservation = removeMissingValues( m_observation );
      return resultObservation;
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, "Failed to clean timeseries" );
      throw new CoreException( status );
    }
  }

  private IObservation removeMissingValues( final IObservation observation ) throws SensorException
  {
    final IAxis[] axes = observation.getAxes();
    final IAxis[] valueAxes = AxisUtils.findValueAxes( axes, true );

    Assert.isTrue( valueAxes.length == 1 );

    final IAxis valueAxis = valueAxes[0];

    final String parameterType = valueAxis.getType();

    switch( parameterType )
    {
      case ITimeseriesConstants.TYPE_RUNOFF:
      case ITimeseriesConstants.TYPE_WATERLEVEL:
        // ignore W and Q for now, old mechanism
        return observation;

      case ITimeseriesConstants.TYPE_RAINFALL:
      case ITimeseriesConstants.TYPE_EVAPORATION:
        return setMissingTo0( observation, valueAxis );

      case ITimeseriesConstants.TYPE_TEMPERATURE:
        return removeMissing( observation, valueAxis );
    }

    return observation;
  }

  private IObservation setMissingTo0( final IObservation observation, final IAxis valueAxis ) throws SensorException
  {
    final IAxis[] axes = observation.getAxes();

    final IAxis statusAxis = AxisUtils.findStatusAxis( axes, valueAxis );
    final IAxis sourceAxis = AxisUtils.findDataSourceAxis( axes, valueAxis );

    final DataSourceHandler dataSourceHandler = new DataSourceHandler( observation.getMetadataList() );
    final String missingIdentifier = IDataSourceItem.SOURCE_MANUAL_CHANGED;
    final int missingSource = dataSourceHandler.addDataSource( missingIdentifier, missingIdentifier );

    final IObservationVisitor visitor = new IObservationVisitor()
    {
      @Override
      public void visit( final IObservationValueContainer container ) throws SensorException
      {
        final Object value = container.get( valueAxis );
        if( isMissingValue( value ) )
        {
          container.set( valueAxis, new Double( 0.0 ) );
          container.set( statusAxis, KalypsoStati.BIT_CHECK );
          container.set( sourceAxis, missingSource );
        }
      }
    };

    observation.accept( visitor, null, 0 );

    return observation;
  }

  private IObservation removeMissing( final IObservation observation, final IAxis valueAxis ) throws SensorException
  {
    final IAxis[] axes = observation.getAxes();

    final MetadataList clonedMetadata = MetadataHelper.clone( observation.getMetadataList() );

    final ITupleModel sourceModel = observation.getValues( null );

    final Collection<Object[]> targetValueCollector = new ArrayList<>( sourceModel.size() );

    final IObservationVisitor visitor = new IObservationVisitor()
    {
      @Override
      public void visit( final IObservationValueContainer container ) throws SensorException
      {
        final Object value = container.get( valueAxis );
        if( !isMissingValue( value ) )
        {
          // do copy value
          final Object[] values = new Object[axes.length];
          for( int i = 0; i < axes.length; i++ )
            values[i] = container.get( axes[i] );
          targetValueCollector.add( values );
        }
      }
    };

    observation.accept( visitor, null, 0 );

    final Object[][] targetValues = targetValueCollector.toArray( new Object[targetValueCollector.size()][] );

    final String href = observation.getHref();
    final String name = observation.getName();
    final ITupleModel targetModel = new SimpleTupleModel( axes, targetValues );

    return new SimpleObservation( href, name, clonedMetadata, targetModel );
  }

  protected boolean isMissingValue( final Object value )
  {
    if( !(value instanceof Number) )
      return true;

    final double dblValue = ((Number) value).doubleValue();
    return Math.abs( -999.0 - dblValue ) < 0.1;
  }
}