/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ui.rrm.internal.simulations.worker;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollectorWithTime;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.NASimulationOperation;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;

/**
 * @author Holger Albert
 */
public class CalculateSimulationWorker implements ICoreRunnableWithProgress
{
  /**
   * The rrm simulation.
   */
  private final RrmSimulation m_rrmSimulation;

  /**
   * The simulation data.
   */
  private final INaSimulationData m_simulationData;

  /**
   * The constructor.
   * 
   * @param rrmSimulation
   *          The rrm simulation.
   * @param simulationData
   *          The simulation data.
   */
  public CalculateSimulationWorker( final RrmSimulation rrmSimulation, final INaSimulationData simulationData )
  {
    m_rrmSimulation = rrmSimulation;
    m_simulationData = simulationData;
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

    /* The status collector. */
    final IStatusCollector collector = new StatusCollectorWithTime( KalypsoUIRRMPlugin.getID() );

    try
    {
      /* Monitor. */
      monitor.beginTask( "Calculating simulation...", 200 );
      monitor.subTask( "Calculating simulation..." );

      /* Calculate. */
      final NASimulationOperation operation = new NASimulationOperation( m_rrmSimulation.getSimulationFolder(), m_simulationData );
      final IStatus calculateStatus = operation.execute( new SubProgressMonitor( monitor, 200 ) );
      collector.add( calculateStatus );
      if( calculateStatus.getSeverity() >= IStatus.ERROR )
        return collector.asMultiStatus( "Error during calculation of the simulation." );

      return collector.asMultiStatus( "Calculation of the simulation was successfull." );
    }
    catch( final Exception ex )
    {
      /* Add the exception to the log. */
      collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex ) );

      return collector.asMultiStatus( "Error during calculation of the simulation." );
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }
}