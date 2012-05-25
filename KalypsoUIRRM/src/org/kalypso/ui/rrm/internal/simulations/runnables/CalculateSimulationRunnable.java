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
package org.kalypso.ui.rrm.internal.simulations.runnables;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;

/**
 * This runnable calculates the simulation.
 * 
 * @author Holger Albert
 */
public class CalculateSimulationRunnable implements ICoreRunnableWithProgress
{
  /**
   * The simulations to calculate.
   */
  private final NAControl[] m_simulations;

  /**
   * True, if the catchment models should be calculated.
   */
  private final boolean m_calculateCatchmentModels;

  /**
   * True, if the start conditions should be calculated.
   */
  private final boolean m_calculateStartConditions;

  /**
   * The constructor.
   * 
   * @param simulations
   *          The simulations to calculate.
   * @param calculateCatchmentModels
   *          True, if the catchment models should be calculated.
   * @param calculateStartConditions
   *          True, if the start conditions should be calculated.
   */
  public CalculateSimulationRunnable( final NAControl[] simulations, final boolean calculateCatchmentModels, final boolean calculateStartConditions )
  {
    m_simulations = simulations;
    m_calculateCatchmentModels = calculateCatchmentModels;
    m_calculateStartConditions = calculateStartConditions;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( IProgressMonitor monitor )
  {
    /* If no monitor is given, take a null progress monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    try
    {
      /* Monitor. */
      monitor.beginTask( "", 1000 );
      monitor.subTask( "" );

      // TODO

      /* Monitor. */
      monitor.worked( 1000 );

      return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "The calculation was finished." );
    }
    catch( final Exception e )
    {
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), e.getLocalizedMessage(), e );
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }
}