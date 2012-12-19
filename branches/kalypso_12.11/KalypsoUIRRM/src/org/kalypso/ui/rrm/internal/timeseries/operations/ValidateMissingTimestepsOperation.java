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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.joda.time.Period;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.ValidateTimestepsVisitor;

import com.google.common.base.Objects;

/**
 * @author Dirk Kuch
 */
public class ValidateMissingTimestepsOperation implements ICoreRunnableWithProgress
{
  private final IObservation m_observation;

  protected boolean m_wait;

  private final Period m_timestep;

  private IObservation m_repaired;

  public ValidateMissingTimestepsOperation( final IObservation observation, final Period timestep )
  {
    m_observation = observation;
    m_timestep = timestep;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final StatusCollector stati = new StatusCollector( KalypsoUIRRMPlugin.getID() );
    m_repaired = doValidate( m_observation, stati );

    return stati.asMultiStatus( Messages.getString("ValidateMissingTimestepsOperation.0") ); //$NON-NLS-1$
  }

  private final IObservation doValidate( final IObservation observation, final StatusCollector stati )
  {
    try
    {
      final ValidateTimestepsVisitor visitor = new ValidateTimestepsVisitor( m_timestep );
      observation.accept( visitor, null, 1 );

      final IStatus status = visitor.getStatus();
      stati.add( status );

      if( !status.isOK() )
      {
        // FIXME: Arrrgggg, what is this job-nonsense here?!

        final RepairMissingTimestepsOperation operation = new RepairMissingTimestepsOperation( observation, m_timestep );
        final RepairObservationJob job = new RepairObservationJob( stati, status, operation );
        job.schedule();

        while( !job.isDone() )
        {
          try
          {
            Thread.sleep( 333 );
          }
          catch( final InterruptedException e )
          {
          }
        }

        return Objects.firstNonNull( operation.getObservation(), observation );
      }

      return observation;
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
      stati.add( IStatus.ERROR, Messages.getString( "ImportTimeseriesOperation_6" ), e ); //$NON-NLS-1$
    }

    return observation;
  }

  public IObservation getObservation( )
  {
    return Objects.firstNonNull( m_repaired, m_observation );
  }
}
