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
package org.kalypso.ui.rrm.internal.simulations.worker;

import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollectorWithTime;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.simulations.SimulationUtilities;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Holger Albert
 */
public class PrepareShorttermSimulationWorker implements ICoreRunnableWithProgress
{
  /**
   * The rrm simulation.
   */
  private final RrmSimulation m_rrmSimulation;

  /**
   * The simulation data. If necessary it will be updated with the lzsim workspace.
   */
  private final INaSimulationData m_simulationData;

  /**
   * The constructor.
   * 
   * @param rrmSimulation
   *          The rrm simulation.
   * @param simulationData
   *          The simulation data. If necessary it will be updated with the lzsim workspace.
   */
  public PrepareShorttermSimulationWorker( final RrmSimulation rrmSimulation, final INaSimulationData simulationData )
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
      monitor.beginTask( "Preparing simulation...", 200 );
      monitor.subTask( "Preparing simulation..." );

      /* Get the some data of the simulation data. */
      final NAControl simulation = m_simulationData.getMetaControl();

      /* Determine, if it is a longterm simulation. */
      final boolean isLongterm = SimulationUtilities.isLongterm( simulation );
      if( !isLongterm )
      {
        /* Update status. */
        collector.add( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), "This is a shortterm simulation." ) );

        /* If a longterm simulation is referenced. */
        final String initialValueSource = simulation.getInitialValueSource();
        if( initialValueSource != null && initialValueSource.length() > 0 )
        {
          /* Update status. */
          collector.add( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), "It does reference a longterm simulation." ) );

          /* Update the simulation data. */
          final IStatus status = updateSimulationData( m_rrmSimulation, m_simulationData, simulation );
          collector.add( status );
        }
        else
        {
          /* Update status. */
          collector.add( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), "It does not reference a longterm simulation." ) );

          /* HINT: In this case the lzsim data is not copied. */
        }

        /* HINT: Shortterm simulations will use an unchanged expertControl.gml later. */
      }

      /* Monitor. */
      monitor.worked( 200 );

      return collector.asMultiStatus( "Peparation of the simulation was successfull." );
    }
    catch( final Exception ex )
    {
      /* Add the exception to the log. */
      collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex ) );

      return collector.asMultiStatus( "Error during preparation of the simulation." );
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  private IStatus updateSimulationData( final RrmSimulation rrmSimulation, final INaSimulationData simulationData, final NAControl control )
  {
    final String sourceSimulationName = control.getInitialValueSource();
    if( StringUtils.isBlank( sourceSimulationName ) )
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), "No longterm simulation is set." );

    final RrmScenario scenario = rrmSimulation.getScenario();
    final IFolder folderSimulations = scenario.getSimulationsFolder();
    final RrmSimulation sourceSimulation = new RrmSimulation( folderSimulations.getFolder( new Path( sourceSimulationName ) ) );
    if( !sourceSimulation.exists() )
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), String.format( "Failed to copy initial values from simulation '%s': Simulation does not exist.", sourceSimulation.getName() ) );

    final IFolder currentSourceFolder = sourceSimulation.getCurrentResultsFolder();
    if( !currentSourceFolder.exists() )
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), String.format( "Failed to copy initial values from simulation '%s': No results available.", sourceSimulation.getName() ) );

    final IFolder initialValuesSourceFolder = sourceSimulation.getCurrentLzimResultFolder();
    final Date startDate = control.getSimulationStart();
    final String initialValuesSourceFilename = new SimpleDateFormat( "yyyyMMdd'.gml'" ).format( startDate );
    final IFile initialValuesSourceFile = initialValuesSourceFolder.getFile( initialValuesSourceFilename );
    if( !initialValuesSourceFile.exists() )
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), String.format( "Failed to copy initial values from simulation '%s': Initial values missing (%s).", sourceSimulation.getName(), initialValuesSourceFilename ) );

    try
    {
      final GMLWorkspace lzsimWorkspace = GmlSerializer.createGMLWorkspace( initialValuesSourceFile, simulationData.getModelWorkspace().getContext(), simulationData.getFeatureProviderFactory(), null );
      simulationData.setLzsimWorkspace( lzsimWorkspace );

      return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), String.format( "Initial values were loaded from simulation '%s'.", sourceSimulation.getName() ) );
    }
    catch( final Exception e )
    {
      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), String.format( "Failed to load initial values from simulation '%s'.", sourceSimulation.getName() ), e );
    }
  }
}