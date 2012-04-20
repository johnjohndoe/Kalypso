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
import org.joda.time.Period;
import org.kalypso.commons.time.PeriodUtils;
import org.kalypso.ogc.sensor.IObservation;

/**
 * @author Dirk Kuch
 */
public class RepairMissingTimestepsOperation implements IRepairObservationWorker
{
  private final Period m_timestep;

  private final IObservation m_observation;

  public RepairMissingTimestepsOperation( final IObservation observation, final Period timestep )
  {
    m_observation = observation;
    m_timestep = timestep;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public String getDialogTitle( )
  {
    return "Fehlwerte wurden festgestellt";
  }

  @Override
  public String getDialogMessage( )
  {
    final String time = PeriodUtils.formatDefault( m_timestep );

    return String.format( "Nicht alle Zeitschritte der %s-Zeitreihe sind vorhanden.\n\nFehlende Einträge automatisch hinzufügen?", time );
  }
}
