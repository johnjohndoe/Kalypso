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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.timeseries.datasource.DataSourceProxyObservation;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.operations.RepairMissingTimestepsOperation;
import org.kalypso.ui.rrm.internal.timeseries.operations.RepairRueckspruengeOperation;
import org.kalypso.ui.rrm.internal.timeseries.operations.RepairTimestampsOperation;
import org.kalypso.ui.rrm.internal.timeseries.operations.ValidateTimestampVisitor;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.ValidateRuecksprungVisitor;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.ValidateTimestepsVisitor;

/**
 * @author Dirk Kuch
 */
public class RepairTimeseriesOperation implements ICoreRunnableWithProgress
{
  private IObservation m_repaired;

  private final IObservation m_observation;

  private final Period m_timestep;

  private final LocalTime m_timestamp;

  private final String m_zmlFile;

  public RepairTimeseriesOperation( final IObservation observation, final Period timestep, final LocalTime timestamp, final String zmlFile )
  {
    m_observation = observation;
    m_timestep = timestep;
    m_timestamp = timestamp;
    m_zmlFile = zmlFile;
  }

  @Override
  public MultiStatus execute( final IProgressMonitor monitor )
  {
    final StatusCollector stati = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    IObservation observation = m_observation;

    // FIXME: bad and very slow
    if( !hasDatasourceAxis( observation ) )
      // DataSourceProxyObservation is badly implemented, it seems that the intrnal observation is created multiple times, wasting memory and time.
      // instead, we should create once a new simple observation, and thats it.
      observation = new DataSourceProxyObservation( observation, m_zmlFile, m_zmlFile, KalypsoStati.BIT_OK );

    observation = doRepairTimestamps( observation, stati, monitor );
    observation = doRepairRueckspruenge( observation, stati, monitor );
    m_repaired = doRepairMissingTimesteps( observation, stati, monitor );

    return stati.asMultiStatus( String.format( Messages.getString( "RepairTimeseriesOperation_0" ), m_zmlFile ) ); //$NON-NLS-1$
  }

  private boolean hasDatasourceAxis( final IObservation observation )
  {
    final IAxis[] axes = observation.getAxes();
    final IAxis[] valueAxes = AxisUtils.findValueAxes( axes );
    final IAxis[] dataSourceAxes = AxisUtils.findDataSourceAxes( axes );

    return ArrayUtils.getLength( valueAxes ) == ArrayUtils.getLength( dataSourceAxes );
  }

  private IObservation doRepairMissingTimesteps( final IObservation observation, final StatusCollector stati, final IProgressMonitor monitor )
  {
    /** don't interpolate over time series with a resolution of 1 second!!! */
    final int minutes = m_timestep.toStandardMinutes().getMinutes();
    if( minutes < 1 )
      return m_observation;

    try
    {
      final ValidateTimestepsVisitor visitor = new ValidateTimestepsVisitor( m_timestep );
      observation.accept( visitor, null, 1 );

      final IStatus status = visitor.getStatus();
      stati.add( status );

      if( !status.isOK() )
      {
        final RepairMissingTimestepsOperation operation = new RepairMissingTimestepsOperation( observation, m_timestep );
        stati.add( operation.execute( monitor ) );

        return Objects.firstNonNull( operation.getObservation(), observation );
      }

      return observation;
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
    }

    return observation;
  }

  private IObservation doRepairRueckspruenge( final IObservation observation, final StatusCollector stati, final IProgressMonitor monitor )
  {
    try
    {
      final ValidateRuecksprungVisitor validator = new ValidateRuecksprungVisitor();
      observation.accept( validator, null, 1 );

      stati.add( validator.getStatus() );

      if( validator.hasRuecksprung() )
      {
        final RepairRueckspruengeOperation operation = new RepairRueckspruengeOperation( observation, validator.getRueckspruenge() );
        stati.add( operation.execute( monitor ) );

        return Objects.firstNonNull( operation.getObservation(), observation );
      }
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
    }

    return observation;
  }

  private IObservation doRepairTimestamps( final IObservation observation, final StatusCollector stati, final IProgressMonitor monitor )
  {
    if( !isTageszeitreihe() )
      return observation;

    try
    {

      final ValidateTimestampVisitor validator = new ValidateTimestampVisitor( m_timestamp );
      observation.accept( validator, null, 1 );

      if( !validator.hasInvalidTimestamps() )
        return observation;

      stati.add( validator.getStatus() );

      final RepairTimestampsOperation operation = new RepairTimestampsOperation( observation, m_timestamp, validator.getInvalidIndices() );
      stati.add( operation.execute( monitor ) );

      return Objects.firstNonNull( operation.getObservation(), observation );
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
    }

    return observation;
  }

  public IObservation getObservation( )
  {
    return Objects.firstNonNull( m_repaired, m_observation );
  }

  private boolean isTageszeitreihe( )
  {
    return Objects.equal( m_timestep, Period.days( 1 ) );
  }
}
