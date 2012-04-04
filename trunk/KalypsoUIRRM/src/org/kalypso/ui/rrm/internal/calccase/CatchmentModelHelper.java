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
package org.kalypso.ui.rrm.internal.calccase;

import java.util.Date;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.joda.time.DateTime;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.model.rcm.binding.IMultiGenerator;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.sensor.DateRange;

/**
 * This class contains functions for dealing with catchment models.
 * 
 * @author Holger Albert
 */
public class CatchmentModelHelper
{
  /**
   * The constructor.
   */
  private CatchmentModelHelper( )
  {
  }

  /**
   * This function executes the catchment model.
   * 
   * @param simulation
   *          The simulation.
   * @param control
   *          The na control.
   * @param model
   *          The na model.
   * @param generator
   *          The rainfall generator.
   * @param targetLink
   *          The target link.
   * @param parameterType
   *          The parameter type.
   * @param monitor
   *          A progress monitor.
   */
  public static void executeCatchmentModel( final RrmSimulation simulation, final NAControl control, final NaModell model, final IRainfallGenerator generator, final QName targetLink, final String parameterType, final IProgressMonitor monitor ) throws CoreException
  {
    AbstractCatchmentModelRunner modelRunner = null;
    if( generator instanceof ILinearSumGenerator )
      modelRunner = new LinearSumCatchmentModelRunner();
    else if( generator instanceof IMultiGenerator )
      modelRunner = new MultiCatchmentModelRunner();
    else
      throw new IllegalArgumentException( "The type of the generator must be that of ILinearSumGenerator or IMultiGenerator..." ); // $NON-NLS-1$

    modelRunner.executeCatchmentModel( simulation, control, model, generator, targetLink, parameterType, monitor );
  }

  /**
   * This function calculates the range for the timeseries to be generated. The range equals the range defined in the
   * simulation adjusted as follows:
   * <ul>
   * <li>1 timestep earlier</li>
   * <li>3 timesteps later</li>
   * </ul>
   * 
   * @param control
   *          The na control.
   * @param timestep
   *          The timestep.
   * @param timestamp
   *          The timestamp.
   * @return The date range.
   */
  public static DateRange getRange( final NAControl control, final Period timestep, final LocalTime timestamp )
  {
    final Date simulationStart = control.getSimulationStart();
    final Date simulationEnd = control.getSimulationEnd();

    final DateTime start = new DateTime( simulationStart );
    final DateTime end = new DateTime( simulationEnd );

    final DateTime adjustedStart = start.minus( timestep );
    final DateTime adjustedEnd = end.plus( timestep ).plus( timestep ).plus( timestep );

    if( timestep.getDays() == 0 || timestamp == null )
      return new DateRange( adjustedStart.toDate(), adjustedEnd.toDate() );

    /* Further adjust range by predefined time. */
    final DateTime startWithTime = adjustedStart.withTime( timestamp.getHourOfDay(), timestamp.getMinuteOfHour(), timestamp.getSecondOfMinute(), timestamp.getMillisOfSecond() );
    final DateTime endWithTime = adjustedEnd.withTime( timestamp.getHourOfDay(), timestamp.getMinuteOfHour(), timestamp.getSecondOfMinute(), timestamp.getMillisOfSecond() );

    /* New start must always be before unadjusted start, fix, if this is not the case. */
    DateTime startWithTimeFixed;
    if( startWithTime.isAfter( adjustedStart ) )
      startWithTimeFixed = startWithTime.minus( timestep );
    else
      startWithTimeFixed = startWithTime;

    /* New end must always be after unadjusted end, fix, if this is not the case. */
    DateTime endWithTimeFixed;
    if( endWithTime.isBefore( adjustedEnd ) )
      endWithTimeFixed = endWithTime.plus( timestep );
    else
      endWithTimeFixed = endWithTime;

    return new DateRange( startWithTimeFixed.toDate(), endWithTimeFixed.toDate() );
  }
}