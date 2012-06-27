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
package org.kalypso.ui.rrm.internal.timeseries.operations;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Dirk Kuch
 */
public class ValidateTageszeitreihenOperation implements ICoreRunnableWithProgress
{
  private IObservation m_observation;

  protected boolean m_wait;

  private final Period m_timestep;

  private final LocalTime m_timestamp;

  public ValidateTageszeitreihenOperation( final IObservation observation, final Period timestep, final LocalTime timestamp )
  {
    m_observation = observation;
    m_timestep = timestep;
    m_timestamp = timestamp;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final StatusCollector stati = new StatusCollector( KalypsoUIRRMPlugin.getID() );
    m_observation = doValidate( m_observation, stati );

    return stati.asMultiStatus( Messages.getString("ValidateTageszeitreihenOperation_0") ); //$NON-NLS-1$
  }

  private final IObservation doValidate( final IObservation observation, final StatusCollector stati )
  {
    if( !isTageszeitreihe() )
    {
      stati.add( IStatus.OK, Messages.getString("ValidateTageszeitreihenOperation_1") ); //$NON-NLS-1$

      return observation;
    }

    try
    {
      final ValidateTimestampVisitor validator = new ValidateTimestampVisitor( m_timestamp );
      observation.accept( validator, null, 1 );

      if( validator.hasInvalidTimestamps() )
      {
        stati.add( validator.getStatus() );

        final RepairTimestampsOperation operation = new RepairTimestampsOperation( observation, m_timestamp, validator.getInvalidIndices() );
        final RepairObservationJob job = new RepairObservationJob( stati, validator.getStatus(), operation );
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
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
      stati.add( IStatus.ERROR, "Zeitstempel-Überprüfung fehlgeschlagen.", e ); //$NON-NLS-1$
    }

    return observation;
  }

  private boolean isTageszeitreihe( )
  {
    return m_timestep.equals( Period.days( 1 ) );
  }

  public IObservation getObservation( )
  {
    return m_observation;
  }
}
