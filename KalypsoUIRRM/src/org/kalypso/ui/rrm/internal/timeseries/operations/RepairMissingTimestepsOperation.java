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
package org.kalypso.ui.rrm.internal.timeseries.operations;

import java.util.Calendar;
import java.util.Date;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.joda.time.Period;
import org.kalypso.commons.time.PeriodUtils;
import org.kalypso.contribs.java.util.CalendarUtilities.FIELD;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.TIMESERIES_TYPE;
import org.kalypso.ogc.sensor.filter.IObservationFilter;
import org.kalypso.ogc.sensor.filter.filters.interval.IntervalDefinition;
import org.kalypso.ogc.sensor.filter.filters.interval.IntervalFilter;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.timeseries.interpolation.InterpolationFilterCreator;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;

import com.google.common.base.Objects;

/**
 * @author Dirk Kuch
 */
public class RepairMissingTimestepsOperation implements IRepairObservationWorker
{
  private final Period m_timestep;

  private final IObservation m_observation;

  private IObservationFilter m_repaired;

  public RepairMissingTimestepsOperation( final IObservation observation, final Period timestep )
  {
    m_observation = observation;
    m_timestep = timestep;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final IAxis[] axes = m_observation.getAxes();
    final IAxis[] valueAxes = AxisUtils.findValueAxes( axes, true );
    if( ArrayUtils.getLength( valueAxes ) != 1 )
      throw new IllegalStateException();

    m_repaired = getFilter( valueAxes[0] );

    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString("RepairMissingTimestepsOperation_0") ); //$NON-NLS-1$
  }

  private IObservationFilter getFilter( final IAxis valueAxes )
  {
    final int amount = m_timestep.toStandardMinutes().getMinutes();

    final TIMESERIES_TYPE type = TIMESERIES_TYPE.getType( valueAxes.getType() );

    try
    {
      switch( type )
      {
        case eSumValue:
        {
          final ITupleModel model = m_observation.getValues( null );
          final Date from = (Date) model.get( 0, AxisUtils.findDateAxis( model.getAxes() ) );
          final Calendar calendar = Calendar.getInstance( KalypsoCorePlugin.getDefault().getTimeZone() );
          calendar.setTime( from );

          final IntervalDefinition definition = new IntervalDefinition( Calendar.MINUTE, amount, 0.0, KalypsoStati.BIT_CHECK, FIELD.HOUR_OF_DAY.toString(), calendar.get( Calendar.HOUR_OF_DAY ) );
          final IntervalFilter filter = new IntervalFilter( definition );
          filter.initFilter( null, m_observation, null );

          return filter;
        }

        default:
          return InterpolationFilterCreator.createFilter( amount, FIELD.MINUTE.toString(), KalypsoStati.BIT_CHECK, "0.0", false, m_observation, null ); //$NON-NLS-1$
      }
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
    }

    throw new UnsupportedOperationException();
  }

  @Override
  public String getDialogTitle( )
  {
    return Messages.getString("RepairMissingTimestepsOperation_2"); //$NON-NLS-1$
  }

  @Override
  public String getDialogMessage( )
  {
    final String time = PeriodUtils.formatDefault( m_timestep );

    return String.format( Messages.getString("RepairMissingTimestepsOperation_3"), time ); //$NON-NLS-1$
  }

  @Override
  public IObservation getObservation( )
  {
    return Objects.firstNonNull( m_repaired, m_observation );
  }
}
