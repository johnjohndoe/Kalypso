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

import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileInfo;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollectorWithTime;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;

import de.renew.workflow.connector.cases.IScenario;

/**
 * This operation compares the selected scenarios with one other scenario.
 * 
 * @author Holger Albert
 */
public class CompareScenariosOperation implements ICoreRunnableWithProgress
{
  /**
   * The scenarios data object.
   */
  private final MergeScenariosData m_scenariosData;

  /**
   * The scenario compare status contains stati for several cases.
   */
  private final ScenarioCompareStatus m_compareStatus;

  /**
   * The constructor.
   * 
   * @param scenariosData
   *          The scenarios data object.
   * @param compareStatus
   *          The scenario compare status contains stati for several cases.
   */
  public CompareScenariosOperation( final MergeScenariosData scenariosData, final ScenarioCompareStatus compareStatus )
  {
    m_scenariosData = scenariosData;
    m_compareStatus = compareStatus;
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
        throw new IllegalArgumentException( "No scenarios selected..." );

      /* Monitor. */
      monitor.beginTask( String.format( "Comparing the scenarios against the scenario '%s'...", targetScenario.getName() ), 750 * selectedScenarios.length );

      /* Get the reference rrm scenario. */
      final IFolder referenceScenariofolder = targetScenario.getFolder();
      final RrmScenario referenceRrmScenario = new RrmScenario( referenceScenariofolder );

      /* Get the files of the reference rrm scenario. */
      final IFile referenceModelFile = referenceRrmScenario.getModelFile();
      final IFile referenceParameterGml = referenceRrmScenario.getParameterGml();
      final IFile referenceHydrotopGml = referenceRrmScenario.getHydrotopGml();

      /* Loop all selected scenarios. */
      for( final IScenario selectedScenario : selectedScenarios )
      {
        /* Get the selected rrm scenario. */
        final IFolder selectedFolder = selectedScenario.getFolder();
        final RrmScenario selectedRrmScenario = new RrmScenario( selectedFolder );

        /* Get the files of the selected rrm scenario. */
        final IFile selectedModelFile = selectedRrmScenario.getModelFile();
        final IFile selectedParameterGml = selectedRrmScenario.getParameterGml();
        final IFile selectedHydrotopGml = selectedRrmScenario.getHydrotopGml();

        /* Monitor. */
        monitor.subTask( "Comparing the model" );

        /* Compare. */
        if( !m_compareStatus.hasStatus( selectedScenario.getURI(), ScenarioCompareStatus.KEY_MODEL ) )
        {
          final IStatus modelStatus = compareFiles( referenceModelFile, selectedModelFile );
          m_compareStatus.putStatus( selectedScenario.getURI(), ScenarioCompareStatus.KEY_MODEL, modelStatus );
        }

        /* Monitor. */
        monitor.worked( 250 );
        monitor.subTask( "Comparing the parameter..." );

        /* Compare. */
        if( !m_compareStatus.hasStatus( selectedScenario.getURI(), ScenarioCompareStatus.KEY_PARAMETER ) )
        {
          final IStatus parameterStatus = compareFiles( referenceParameterGml, selectedParameterGml );
          m_compareStatus.putStatus( selectedScenario.getURI(), ScenarioCompareStatus.KEY_PARAMETER, parameterStatus );
        }

        /* Monitor. */
        monitor.worked( 250 );
        monitor.subTask( "Comparing the hydrotopes..." );

        /* Compare. */
        if( !m_compareStatus.hasStatus( selectedScenario.getURI(), ScenarioCompareStatus.KEY_HYDROTOPES ) )
        {
          final IStatus hydrotopeStatus = compareFiles( referenceHydrotopGml, selectedHydrotopGml );
          m_compareStatus.putStatus( selectedScenario.getURI(), ScenarioCompareStatus.KEY_HYDROTOPES, hydrotopeStatus );
        }

        /* Monitor. */
        monitor.worked( 250 );
      }

      return collector.asMultiStatus( String.format( "Comparing the scenarios against the scenario '%s' succeeded.", targetScenario.getName() ) );
    }
    catch( final Exception ex )
    {
      collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex ) );
      return collector.asMultiStatus( String.format( "Comparing the scenarios against the scenario '%s' failed.", targetScenario.getName() ) );
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  private IStatus compareFiles( final IFile referenceModelFile, final IFile selectedModelFile ) throws CoreException
  {
    final IFileStore referenceStore = EFS.getStore( referenceModelFile.getLocationURI() );
    final IFileInfo referenceFileInfo = referenceStore.fetchInfo();
    final long referenceLength = referenceFileInfo.getLength();

    final IFileStore selectedStore = EFS.getStore( selectedModelFile.getLocationURI() );
    final IFileInfo selectedFileInfo = selectedStore.fetchInfo();
    final long selectedLength = selectedFileInfo.getLength();

    if( referenceLength != selectedLength )
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), "Changed" );

    // TODO Eventually other checks...

    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "Not changed" );
  }
}