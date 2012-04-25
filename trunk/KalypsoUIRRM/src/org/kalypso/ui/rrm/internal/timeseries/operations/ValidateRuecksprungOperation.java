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
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.ValidateRuecksprungVisitor;

import com.google.common.base.Objects;

/**
 * @author Dirk Kuch
 */
public class ValidateRuecksprungOperation implements ICoreRunnableWithProgress
{
  private IObservation m_observation;

  protected boolean m_wait;

  public ValidateRuecksprungOperation( final IObservation observation )
  {
    m_observation = observation;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final StatusCollector stati = new StatusCollector( KalypsoUIRRMPlugin.getID() );
    m_observation = doValidate( m_observation, stati );

    return stati.asMultiStatus( Messages.getString("ValidateRuecksprungOperation.0") ); //$NON-NLS-1$
  }

  private final IObservation doValidate( final IObservation observation, final StatusCollector stati )
  {
    try
    {
      final ValidateRuecksprungVisitor validator = new ValidateRuecksprungVisitor();
      observation.accept( validator, null, 1 );

      if( validator.hasRuecksprung() )
      {
        stati.add( validator.getStatus() );

        final RepairRueckspruengeOperation operation = new RepairRueckspruengeOperation( observation, validator.getRueckspruenge() );
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
      stati.add( IStatus.ERROR, Messages.getString( "ImportTimeseriesOperation_9" ), e ); //$NON-NLS-1$
    }

    return observation;
  }

  public IObservation getObservation( )
  {
    return m_observation;
  }
}
