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
package org.kalypso.ui.rrm.internal.scenarios;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.resources.CollectFolderVisitor;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollectorWithTime;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;

import de.renew.workflow.connector.cases.IScenario;

/**
 * This operation merges the selected scenarios into one other scenario.
 * 
 * @author Holger Albert
 */
public class MergeScenariosOperation implements ICoreRunnableWithProgress
{
  /**
   * The scenario, where the others scenarios should be merged into.
   */
  private final IScenario m_scenario;

  /**
   * The scenarios data object.
   */
  private final MergeScenariosData m_scenariosData;

  /**
   * The constructor.
   * 
   * @param scenario
   *          The scenario, where the others scenarios should be merged into.
   * @param scenariosData
   *          The scenarios data object.
   */
  public MergeScenariosOperation( final IScenario scenario, final MergeScenariosData scenariosData )
  {
    m_scenario = scenario;
    m_scenariosData = scenariosData;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( IProgressMonitor monitor )
  {
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    /* The status collector. */
    final IStatusCollector collector = new StatusCollectorWithTime( KalypsoUIRRMPlugin.getID() );

    try
    {
      /* Get the selected scenarios. */
      final IScenario[] selectedScenarios = m_scenariosData.getSelectedScenarios();
      if( selectedScenarios == null || selectedScenarios.length == 0 )
        throw new IllegalArgumentException( "No scenarios selected..." );

      /* Monitor. */
      monitor.beginTask( String.format( "Merging the scenarios into the scenario '%s'...", m_scenario.getName() ), 1500 * selectedScenarios.length );

      /* Get the simulations folder of the target scenario. */
      final IFolder scenarioFolder = m_scenario.getFolder();
      final RrmScenario rrmScenario = new RrmScenario( scenarioFolder );
      final IFolder simulationsFolder = rrmScenario.getSimulationsFolder();

      /* Loop all selected scenarios. */
      for( final IScenario selectedScenario : selectedScenarios )
      {
        /* Monitor. */
        monitor.subTask( String.format( "Importing scenario '%s'...", selectedScenario.getName() ) );

        /* Get the simulations folder of the source scenario. */
        final IFolder selectedScenarioFolder = selectedScenario.getFolder();
        final RrmScenario selectedRrmScenario = new RrmScenario( selectedScenarioFolder );
        final IFolder selectedSimulationsFolder = selectedRrmScenario.getSimulationsFolder();

        /* Monitor. */
        monitor.worked( 250 );
        monitor.subTask( "Copying simulations..." );

        /* Copy the simulations. */
        copySimulations( simulationsFolder, selectedSimulationsFolder );

        /* Monitor. */
        monitor.worked( 250 );
        monitor.subTask( "Updating catchment models..." );

        /* Update the catchment models. */
        updateCatchmentModels();

        /* Monitor. */
        monitor.worked( 250 );
        monitor.subTask( "Updating timeseries mappings..." );

        /* Update the timeseries mappings. */
        updateTimeseriesMappings();

        /* Monitor. */
        monitor.worked( 250 );
        monitor.subTask( "Updating simulations..." );

        /* Update the simulations. */
        updateSimulations();

        /* Monitor. */
        monitor.worked( 250 );
        monitor.subTask( "Cleaning up..." );

        /* Should the selected scenario be deleted? */
        final boolean deleteScenarios = m_scenariosData.isDeleteScenarios();

        /* Clean up the scenario. */
        cleanUpScenario( selectedScenario, deleteScenarios );

        /* Monitor. */
        monitor.worked( 250 );
      }

      return collector.asMultiStatus( String.format( "Merging the scenarios into the scenario '%s' succeeded.", m_scenario.getName() ) );
    }
    catch( final Exception ex )
    {
      collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex ) );
      return collector.asMultiStatus( String.format( "Merging the scenarios into the scenario '%s' failed.", m_scenario.getName() ) );
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  /**
   * This function copies the contained simulations in the selected simulations folder into the simulations folder.
   * 
   * @param simulationsFolder
   *          The simulations folder of the target scenario.
   * @param selectedSimulationsFolder
   *          The simulations folder of the source scenario.
   */
  private void copySimulations( final IFolder simulationsFolder, final IFolder selectedSimulationsFolder ) throws CoreException
  {
    /* Get the simulations. */
    final CollectFolderVisitor selectedFolderVisitor = new CollectFolderVisitor( new IFolder[] {} );
    selectedSimulationsFolder.accept( selectedFolderVisitor );
    final IFolder[] selectedFolders = selectedFolderVisitor.getFolders();
    for( final IFolder selectedFolder : selectedFolders )
    {
      final IFolder targetFolder = simulationsFolder.getFolder( selectedFolder.getName() );
      if( targetFolder.exists() )
      {
        // TODO
      }

      // TODO
    }
  }

  private void updateCatchmentModels( )
  {
    // TODO
  }

  private void updateTimeseriesMappings( )
  {
    // TODO
  }

  private void updateSimulations( )
  {
    // TODO The new ids of the catchment models generators must be set into the new simulations...
    // TODO
  }

  private void cleanUpScenario( final IScenario selectedScenario, final boolean deleteScenarios )
  {
    // TODO
  }
}