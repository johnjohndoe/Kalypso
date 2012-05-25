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

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusWithTime;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;

/**
 * This runnable calculates the simulation.
 * 
 * @author Holger Albert
 */
public class CalculateSimulationRunnable implements ICoreRunnableWithProgress
{
  /**
   * The date formatter.
   */
  private static final DateFormat DF = new SimpleDateFormat( "dd.MM.yy HH:mm" );

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

    /* The status collector. */
    final IStatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    try
    {
      /* Monitor. */
      monitor.beginTask( "Calculating simulations...", 1000 * m_simulations.length );
      monitor.subTask( "Calculating simulations..." );

      /* Sort the simulations. */
      /* Longterm simulations should be calculated first. */
      Arrays.sort( m_simulations, new SimulationComparator() );

      /* Calculate the simulations. */
      for( final NAControl simulation : m_simulations )
      {
        final IStatus status = calculateSimulation( simulation, new SubProgressMonitor( monitor, 1000 ) );
        collector.add( status );
      }

      return collector.asMultiStatus( "The calculation was finished." );
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

  /**
   * This function calculates one simulation.
   * 
   * @param simulation
   *          The simulation to calculate.
   * @param monitor
   *          A progress monitor.
   * @return A status with the result of the calculation.
   */
  private IStatus calculateSimulation( final NAControl simulation, final IProgressMonitor monitor )
  {
    /* The status collector. */
    final IStatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    try
    {
      /* Monitor. */
      monitor.beginTask( String.format( "Calculating simulation '%s'...", simulation.getDescription() ), 1000 );
      monitor.subTask( "Checking preconditions..." );

      /* Mark a status with the current time. */
      final Date startTime = new Date();
      collector.add( new StatusWithTime( IStatus.OK, KalypsoUIRRMPlugin.getID(), String.format( "Calculation started at %s.", DF.format( startTime ) ), startTime ) );

      /* Copy given flags. */
      boolean calculateCatchmentModels = m_calculateCatchmentModels;
      final boolean calculateStartConditions = m_calculateStartConditions;

      /* Get the rrm simulation. */
      final RrmSimulation rrmSimulation = getRrmSimulation( simulation );

      /* Create/Cleanup the simulation folder. */
      final IFolder simulationFolder = rrmSimulation.getSimulationFolder();
      if( simulationFolder.exists() )
      {
        /* HINT: We keep the selection of the user. */

        /* Clean up. */
        // TODO
      }
      else
      {
        /* The catchment models must be calculated. */
        /* This overrides the selection of the user. */
        calculateCatchmentModels = true;

        /* Create. */
        // TODO
      }

      /* Monitor. */
      monitor.worked( 200 );
      monitor.subTask( "Copy some data..." );

      /* Copy some data. */
      // TODO

      /* Monitor. */
      monitor.worked( 200 );
      monitor.subTask( "Calculate the catchment models..." );

      /* Calculate the catchment models. */
      // TODO

      /* Monitor. */
      monitor.worked( 200 );
      monitor.subTask( "Calculate the simulation..." );

      /* If this is a longterm simulation and the start conditions should be calculated, */
      /* We need to manipulate the modell.gml, activating all result flags. */
      // TODO

      /* Calculate the simulation. */
      // TODO

      /* Monitor. */
      monitor.worked( 200 );
      monitor.subTask( "Save the log..." );

      /* Save the status in the simulation folder. */
      // TODO

      /* Mark a status with the current time. */
      final Date endTime = new Date();
      collector.add( new StatusWithTime( IStatus.OK, KalypsoUIRRMPlugin.getID(), String.format( "Calculation finished at %s.", DF.format( endTime ) ), endTime ) );

      /* Monitor. */
      monitor.worked( 200 );

      return collector.asMultiStatus( String.format( "Calculation of '%s' finished without errors.", simulation.getDescription() ) );
    }
    catch( final Exception ex )
    {
      /* Add the exception to the log. */
      collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex ) );

      return collector.asMultiStatus( String.format( "Calculation of '%s' finished with errors.", simulation.getDescription() ) );
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  /**
   * This function returns the rrm simulation.
   * 
   * @param simulation
   *          The simulation to calculate.
   */
  private RrmSimulation getRrmSimulation( final NAControl simulation ) throws CoreException
  {
    /* Get the description of the simulation. */
    final String description = simulation.getDescription();

    /* Get the folder of the simulation. */
    final SzenarioDataProvider dataProvider = ScenarioHelper.getScenarioDataProvider();
    final IContainer scenarioFolder = dataProvider.getScenarioFolder();
    final IFolder calcCasesFolder = scenarioFolder.getFolder( new Path( INaProjectConstants.FOLDER_RECHENVARIANTEN ) );
    final IFolder simulationFolder = calcCasesFolder.getFolder( description );

    return new RrmSimulation( simulationFolder );
  }
}