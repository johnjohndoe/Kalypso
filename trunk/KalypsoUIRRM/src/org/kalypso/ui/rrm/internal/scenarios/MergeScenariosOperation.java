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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollectorWithTime;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;

import de.renew.workflow.connector.cases.IScenario;
import de.renew.workflow.connector.cases.IScenarioList;
import de.renew.workflow.connector.cases.IScenarioManager;
import de.renew.workflow.connector.cases.ScenarioHandlingProjectNature;

/**
 * This operation merges the selected scenarios into one other scenario.
 *
 * @author Holger Albert
 */
public class MergeScenariosOperation implements ICoreRunnableWithProgress
{
  /**
   * The scenarios data object.
   */
  private final MergeScenariosData m_scenariosData;

  /**
   * The constructor.
   *
   * @param scenariosData
   *          The scenarios data object.
   */
  public MergeScenariosOperation( final MergeScenariosData scenariosData )
  {
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

    /* Get the target scenario. */
    final IScenario targetScenario = m_scenariosData.getTargetScenario();

    try
    {
      /* Get the selected scenarios. */
      final IScenario[] selectedScenarios = m_scenariosData.getSelectedScenarios();
      if( selectedScenarios == null || selectedScenarios.length == 0 )
        throw new IllegalArgumentException( Messages.getString( "MergeScenariosOperation_0" ) ); //$NON-NLS-1$

      /* Monitor. */
      monitor.beginTask( String.format( Messages.getString( "MergeScenariosOperation_1" ), targetScenario.getName() ), 1250 * selectedScenarios.length ); //$NON-NLS-1$

      /* Get the simulations folder of the target scenario. */
      final IFolder scenarioFolder = targetScenario.getFolder();
      final RrmScenario rrmScenario = new RrmScenario( scenarioFolder );
      final IFolder simulationsFolder = rrmScenario.getSimulationsFolder();

      /* Handle the catchment models and the timeseries mappings. */
      final MergeMappingsWorker mappingsWorker = new MergeMappingsWorker( selectedScenarios, targetScenario );

      /* Analyze. */
      final IStatus analyzeStatus = mappingsWorker.analyze( new SubProgressMonitor( monitor, 250 * selectedScenarios.length ) );
      collector.add( analyzeStatus );

      /* Create mappings. */
      final IStatus mappingsStatus = mappingsWorker.createMappings( new SubProgressMonitor( monitor, 250 * selectedScenarios.length ) );
      collector.add( mappingsStatus );

      /* Import the simulations. */
      final Map<String, IFolder> newSimulationFolders = importSimulations( selectedScenarios, simulationsFolder, monitor );

      /* Create simulations. */
      final IStatus simulationsStatus = mappingsWorker.createSimulations( newSimulationFolders, new SubProgressMonitor( monitor, 250 * selectedScenarios.length ) );
      collector.add( simulationsStatus );

      /* Should the selected scenarios be deleted? */
      final boolean deleteScenarios = m_scenariosData.isDeleteScenarios();

      /* Clean up the scenarios. */
      cleanUpScenarios( selectedScenarios, deleteScenarios, monitor );

      return collector.asMultiStatus( String.format( Messages.getString( "MergeScenariosOperation_2" ), targetScenario.getName() ) ); //$NON-NLS-1$
    }
    catch( final Exception ex )
    {
      collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex ) );
      return collector.asMultiStatus( String.format( Messages.getString( "MergeScenariosOperation_3" ), targetScenario.getName() ) ); //$NON-NLS-1$
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
   * @return A map <scenario-uri_simulation-name, new simulation folder>.
   */
  private Map<String, IFolder> importSimulations( final IScenario[] selectedScenarios, final IFolder simulationsFolder, final IProgressMonitor monitor ) throws CoreException
  {
    /* Store the new simulation folders. Needed for updating the description of a simulation later. */
    final Map<String, IFolder> newSimulationFolders = new HashMap<>();

    /* Loop all selected scenarios. */
    for( final IScenario selectedScenario : selectedScenarios )
    {
      /* Monitor. */
      monitor.subTask( String.format( Messages.getString( "MergeScenariosOperation_4" ), selectedScenario.getName() ) ); //$NON-NLS-1$

      /* Get the simulations folder of the source scenario. */
      final IFolder selectedScenarioFolder = selectedScenario.getFolder();
      final RrmScenario selectedRrmScenario = new RrmScenario( selectedScenarioFolder );
      final IFolder selectedSimulationsFolder = selectedRrmScenario.getSimulationsFolder();

      /* Copy the simulations. */
      copySimulations( selectedScenario, selectedSimulationsFolder, simulationsFolder, newSimulationFolders );

      /* Monitor. */
      monitor.worked( 250 );
    }

    return newSimulationFolders;
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
   * @param newSimulationFolders
   *          A map <scenario-uri_simulation-name, new simulation folder>.
   */
  private void copySimulations( final IScenario selectedScenario, final IFolder selectedSimulationsFolder, final IFolder simulationsFolder, final Map<String, IFolder> newSimulationFolders ) throws CoreException
  {
    /* Get the simulations. */
    final IResource[] selectedResources = selectedSimulationsFolder.members();
    for( final IResource selectedResource : selectedResources )
    {
      /* Ignore resources, that are no folder. */
      if( selectedResource.getType() != IResource.FOLDER )
        continue;

      /* Get the selected folder. */
      final IFolder selectedFolder = (IFolder) selectedResource;

      /* Get the target folder. */
      final IFolder targetFolder = getTargetFolder( selectedScenario, selectedFolder, simulationsFolder );

      /* Copy the simulation. */
      ResourceUtilities.copyFolderContents( selectedFolder, targetFolder );

      /* Update the map. */
      final String key = String.format( "%s_%s", selectedScenario.getURI(), selectedFolder.getName() ); //$NON-NLS-1$
      newSimulationFolders.put( key, targetFolder );
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

    final IFolder targetFolder1 = simulationsFolder.getFolder( String.format( "%s (aus %s)", selectedFolder.getName(), selectedScenario.getName() ) ); //$NON-NLS-1$
    if( !targetFolder1.exists() )
      return targetFolder1;

    int cnt = 1;
    IFolder targetFolder2 = simulationsFolder.getFolder( String.format( "%s (aus %s) %d", selectedFolder.getName(), selectedScenario.getName(), cnt++ ) ); //$NON-NLS-1$
    while( targetFolder2.exists() )
      targetFolder2 = simulationsFolder.getFolder( String.format( "%s (aus %s) %d", selectedFolder.getName(), selectedScenario.getName(), cnt++ ) ); //$NON-NLS-1$

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
  private void cleanUpScenarios( final IScenario[] selectedScenarios, final boolean deleteScenarios, final IProgressMonitor monitor ) throws CoreException
  {
    /* If the selected scenarios should not be deleted, return. */
    if( !deleteScenarios )
      return;

    /* Loop all selected scenarios. */
    for( final IScenario selectedScenario : selectedScenarios )
    {
      /* Monitor. */
      monitor.subTask( String.format( Messages.getString( "MergeScenariosOperation_9" ), selectedScenario.getName() ) ); //$NON-NLS-1$

      /* Continue, if there are derived scenarios. */
      final IScenarioList derivedScenarios = selectedScenario.getDerivedScenarios();
      if( derivedScenarios != null && derivedScenarios.getScenarios().size() > 0 )
        continue;

      /* Remove the scenario. */
      final IProject project = selectedScenario.getProject();
      final ScenarioHandlingProjectNature nature = ScenarioHandlingProjectNature.toThisNature( project );
      final IScenarioManager scenarioManager = nature.getCaseManager();
      scenarioManager.removeCase( selectedScenario, new SubProgressMonitor( monitor, 250 ) );
    }
  }
}