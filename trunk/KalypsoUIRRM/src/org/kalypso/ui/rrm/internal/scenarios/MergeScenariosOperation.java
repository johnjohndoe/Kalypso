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
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.core.resources.CollectFolderVisitor;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
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
      monitor.beginTask( String.format( "Merging the scenarios into the scenario '%s'...", m_scenario.getName() ), 1250 * selectedScenarios.length );

      /* Get the simulations folder of the target scenario. */
      final IFolder scenarioFolder = m_scenario.getFolder();
      final RrmScenario rrmScenario = new RrmScenario( scenarioFolder );
      final IFolder simulationsFolder = rrmScenario.getSimulationsFolder();

      /* Handle the catchment models and the timeseries mappings. */
      final MergeMappingsWorker mappingsWorker = new MergeMappingsWorker( selectedScenarios, m_scenario );

      /* Analyze. */
      final IStatus analyzeStatus = mappingsWorker.analyze( new SubProgressMonitor( monitor, 250 * selectedScenarios.length ) );
      collector.add( analyzeStatus );

      /* Create mappings. */
      final IStatus mappingsStatus = mappingsWorker.createMappings( new SubProgressMonitor( monitor, 250 * selectedScenarios.length ) );
      collector.add( mappingsStatus );

      /* Create simulations. */
      final IStatus simulationsStatus = mappingsWorker.createSimulations( new SubProgressMonitor( monitor, 250 * selectedScenarios.length ) );
      collector.add( simulationsStatus );

      /* Import the simulations. */
      importSimulations( selectedScenarios, simulationsFolder, monitor );

      /* Should the selected scenarios be deleted? */
      final boolean deleteScenarios = m_scenariosData.isDeleteScenarios();

      /* Clean up the scenarios. */
      cleanUpScenarios( selectedScenarios, deleteScenarios, monitor );

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
   * This function imports all simulations of the selected scenarios and copies them into the simulations folder of the
   * target scenario.
   * 
   * @param selectedScenarios
   *          The source scenarios.
   * @param simulationsFolder
   *          The simulations folder of the target scenario.
   * @param monitor
   *          A progress monitor.
   */
  private void importSimulations( final IScenario[] selectedScenarios, final IFolder simulationsFolder, final IProgressMonitor monitor ) throws CoreException
  {
    /* Loop all selected scenarios. */
    for( final IScenario selectedScenario : selectedScenarios )
    {
      /* Monitor. */
      monitor.subTask( String.format( "Copying simulations of scenario '%s'...", selectedScenario.getName() ) );

      /* Get the simulations folder of the source scenario. */
      final IFolder selectedScenarioFolder = selectedScenario.getFolder();
      final RrmScenario selectedRrmScenario = new RrmScenario( selectedScenarioFolder );
      final IFolder selectedSimulationsFolder = selectedRrmScenario.getSimulationsFolder();

      /* Copy the simulations. */
      copySimulations( selectedScenario, selectedSimulationsFolder, simulationsFolder );

      /* Monitor. */
      monitor.worked( 250 );
    }
  }

  /**
   * This function copies the contained simulations in the selected simulations folder into the simulations folder.
   * 
   * @param selectedScenario
   *          The source scenario.
   * @param selectedSimulationsFolder
   *          The simulations folder of the source scenario.
   * @param simulationsFolder
   *          The simulations folder of the target scenario.
   */
  private void copySimulations( final IScenario selectedScenario, final IFolder selectedSimulationsFolder, final IFolder simulationsFolder ) throws CoreException
  {
    /* Get the simulations. */
    final CollectFolderVisitor selectedFolderVisitor = new CollectFolderVisitor( new IFolder[] {} );
    selectedSimulationsFolder.accept( selectedFolderVisitor );
    final IFolder[] selectedFolders = selectedFolderVisitor.getFolders();
    for( final IFolder selectedFolder : selectedFolders )
    {
      /* Get the target folder. */
      final IFolder targetFolder = getTargetFolder( selectedScenario, selectedFolder, simulationsFolder );

      /* Copy the simulation. */
      ResourceUtilities.copyFolderContents( selectedFolder, targetFolder );
    }
  }

  /**
   * This function returns the target folder. It makes sure that it does not exist.
   * 
   * @param selectedScenario
   *          The source scenario.
   * @param selectedFolder
   *          The folder of one simulation in the source scenario.
   * @param simulationsFolder
   *          The simulations folder of the target scenario.
   * @return The target folder.
   */
  private IFolder getTargetFolder( final IScenario selectedScenario, final IFolder selectedFolder, final IFolder simulationsFolder )
  {
    final IFolder targetFolder = simulationsFolder.getFolder( selectedFolder.getName() );
    if( !targetFolder.exists() )
      return targetFolder;

    final IFolder targetFolder1 = simulationsFolder.getFolder( String.format( "%s (aus %s)", selectedFolder.getName(), selectedScenario.getName() ) );
    if( !targetFolder1.exists() )
      return targetFolder1;

    int cnt = 1;
    IFolder targetFolder2 = simulationsFolder.getFolder( String.format( "%s (aus %s) %d", selectedFolder.getName(), selectedScenario.getName(), cnt++ ) );
    while( targetFolder2.exists() )
      targetFolder2 = simulationsFolder.getFolder( String.format( "%s (aus %s) %d", selectedFolder.getName(), selectedScenario.getName(), cnt++ ) );

    return targetFolder2;
  }

  /**
   * This function removes the selected scenarios.
   * 
   * @param selectedScenarios
   *          The source scenarios.
   * @param deleteScenarios
   *          True, if the imported scenarios should be deleted.
   * @param monitor
   *          A progress monitor.
   */
  private void cleanUpScenarios( final IScenario[] selectedScenarios, final boolean deleteScenarios, final IProgressMonitor monitor )
  {
    /* If the selected scenarios should not be deleted, return. */
    if( !deleteScenarios )
      return;

    /* Loop all selected scenarios. */
    for( final IScenario selectedScenario : selectedScenarios )
    {
      /* Monitor. */
      monitor.subTask( String.format( "Cleaning up scenario '%s'...", selectedScenario.getName() ) );

      // TODO

      /* Monitor. */
      monitor.worked( 250 );
    }
  }
}