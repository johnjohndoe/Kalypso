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
package org.kalypso.ui.rrm.internal.simulations.runnables;

import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.ui.dialogs.ContainerGenerator;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollectorWithTime;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.NaSimulationDataFactory;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.control.NAModellControl;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.simulations.SimulationAccessor;
import org.kalypso.ui.rrm.internal.simulations.SimulationUtilities;
import org.kalypso.ui.rrm.internal.simulations.worker.CalculateCatchmentModelsWorker;
import org.kalypso.ui.rrm.internal.simulations.worker.CalculateSimulationWorker;
import org.kalypso.ui.rrm.internal.simulations.worker.CleanupSimulationWorker;
import org.kalypso.ui.rrm.internal.simulations.worker.CreateSimulationWorker;
import org.kalypso.ui.rrm.internal.simulations.worker.PrepareLongtermSimulationWorker;
import org.kalypso.ui.rrm.internal.simulations.worker.PrepareShorttermSimulationWorker;
import org.kalypso.utils.log.GeoStatusLog;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

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
    final IStatusCollector collector = new StatusCollectorWithTime( KalypsoUIRRMPlugin.getID() );

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
    final IStatusCollector collector = new StatusCollectorWithTime( KalypsoUIRRMPlugin.getID() );

    /* The rrm simulation. */
    RrmSimulation rrmSimulation = null;

    /* The simulation data. */
    INaSimulationData simulationData = null;

    /* The workspace of the simulation. */
    GMLWorkspace simulationWorkspace = null;

    try
    {
      /* Monitor. */
      monitor.beginTask( String.format( "Calculating simulation '%s'...", simulation.getDescription() ), 1000 );
      monitor.subTask( "Checking preconditions..." );

      /* Mark a status with the current time. */
      final Date startTime = new Date();
      collector.add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), String.format( "Calculation started at %s.", DF.format( startTime ) ) ) );

      /* Get the rrm simulation and rrm scenario. */
      rrmSimulation = getRrmSimulation( simulation );
      final RrmScenario rrmScenario = rrmSimulation.getScenario();

      /* Copy given flags. */
      boolean calculateCatchmentModels = m_calculateCatchmentModels;
      final boolean calculateStartConditions = m_calculateStartConditions;

      /* Create/Cleanup the simulation folder. */
      final IFolder simulationFolder = rrmSimulation.getSimulationFolder();
      if( simulationFolder.exists() )
      {
        /* HINT: We keep the selection of the user. */

        /* Clean up. */
        final CleanupSimulationWorker cleanupWorker = new CleanupSimulationWorker( rrmSimulation, calculateCatchmentModels );
        final IStatus cleanupStatus = cleanupWorker.execute( new SubProgressMonitor( monitor, 200 ) );
        collector.add( cleanupStatus );
        if( cleanupStatus.getSeverity() >= IStatus.ERROR )
          return collector.asMultiStatus( String.format( "Calculation of '%s' finished with errors.", simulation.getDescription() ) );
      }
      else
      {
        /* The catchment models must be calculated. */
        /* This overrides the selection of the user. */
        calculateCatchmentModels = true;

        /* Create. */
        final CreateSimulationWorker createWorker = new CreateSimulationWorker( rrmSimulation );
        final IStatus createStatus = createWorker.execute( new SubProgressMonitor( monitor, 200 ) );
        collector.add( createStatus );
        if( createStatus.getSeverity() >= IStatus.ERROR )
          return collector.asMultiStatus( String.format( "Calculation of '%s' finished with errors.", simulation.getDescription() ) );
      }

      /* Monitor. */
      monitor.subTask( "Preparing..." );

      /* Create the URLs. */
      final URL modelURL = ResourceUtilities.createURL( rrmScenario.getModelFile() );
      final URL controlURL = ResourceUtilities.createURL( rrmScenario.getExpertControlGml() );
      final URL parameterURL = ResourceUtilities.createURL( rrmScenario.getParameterGml() );
      final URL hydrotopURL = ResourceUtilities.createURL( rrmScenario.getHydrotopGml() );
      final URL sudsURL = ResourceUtilities.createURL( rrmScenario.getSudsGml() );
      final URL syntNURL = ResourceUtilities.createURL( rrmScenario.getSyntnGml() );
      final URL catchmentModelsUrl = ResourceUtilities.createURL( rrmScenario.getCatchmentModelsGml() );
      final URL timeseriesMappingsUrl = ResourceUtilities.createURL( rrmScenario.getTimeseriesMappingsGml() );

      /* Load all simulation data. */
      simulationData = NaSimulationDataFactory.load( modelURL, controlURL, null, parameterURL, hydrotopURL, sudsURL, syntNURL, null, catchmentModelsUrl, timeseriesMappingsUrl, null, null );

      /* Clone the simulation. */
      simulationWorkspace = FeatureFactory.createGMLWorkspace( simulation.getFeatureType(), modelURL, simulationData.getFeatureProviderFactory() );
      final NAControl simulationFeature = (NAControl) simulationWorkspace.getRootFeature();
      FeatureHelper.copyData( simulation, simulationFeature );

      /* Set the meta control to the simulation data. */
      simulationData.setMetaControl( simulationFeature );

      /* Prepare longterm/shortterm simulation. */
      final boolean isLongterm = SimulationUtilities.isLongterm( simulation );
      if( isLongterm )
      {
        /* Prepare longterm simulation. */
        final PrepareLongtermSimulationWorker prepareLongtermWorker = new PrepareLongtermSimulationWorker( calculateStartConditions, simulationData, new SimulationAccessor( simulation ) );
        final IStatus prepareLongtermStatus = prepareLongtermWorker.execute( new SubProgressMonitor( monitor, 200 ) );
        collector.add( prepareLongtermStatus );
        if( prepareLongtermStatus.getSeverity() >= IStatus.ERROR )
          return collector.asMultiStatus( String.format( "Calculation of '%s' finished with errors.", simulation.getDescription() ) );
      }
      else
      {
        /* Prepare shortterm simulation. */
        final PrepareShorttermSimulationWorker prepareShorttermWorker = new PrepareShorttermSimulationWorker( rrmSimulation, simulationData );
        final IStatus prepareShorttermStatus = prepareShorttermWorker.execute( new SubProgressMonitor( monitor, 200 ) );
        collector.add( prepareShorttermStatus );
        if( prepareShorttermStatus.getSeverity() >= IStatus.ERROR )
          return collector.asMultiStatus( String.format( "Calculation of '%s' finished with errors.", simulation.getDescription() ) );
      }

      /* Monitor. */
      monitor.subTask( "Calculating the catchment models..." );

      /* Calculate the catchment models. */
      final CalculateCatchmentModelsWorker catchmentModelsWorker = new CalculateCatchmentModelsWorker( rrmSimulation, calculateCatchmentModels, simulationData );
      final IStatus catchmentModelsStatus = catchmentModelsWorker.execute( new SubProgressMonitor( monitor, 200 ) );
      collector.add( catchmentModelsStatus );
      if( catchmentModelsStatus.getSeverity() >= IStatus.ERROR )
        return collector.asMultiStatus( String.format( "Calculation of '%s' finished with errors.", simulation.getDescription() ) );

      /* Monitor. */
      monitor.subTask( "Calculating the simulation..." );

      /* Calculate the simulation. */
      final CalculateSimulationWorker calculateWorker = new CalculateSimulationWorker( rrmSimulation, simulationData );
      final IStatus calculateStatus = calculateWorker.execute( new SubProgressMonitor( monitor, 200 ) );
      collector.add( calculateStatus );
      if( calculateStatus.getSeverity() >= IStatus.ERROR )
        return collector.asMultiStatus( String.format( "Calculation of '%s' finished with errors.", simulation.getDescription() ) );

      /* Monitor. */
      monitor.subTask( "Finishing..." );

      /* Mark a status with the current time. */
      final Date endTime = new Date();
      collector.add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), String.format( "Calculation finished at %s.", DF.format( endTime ) ) ) );

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
      /* Save a copy. */
      saveCopy( rrmSimulation, simulationData, simulationWorkspace );

      /* Save the log. */
      saveLog( simulation, rrmSimulation, collector );

      /* Monitor. */
      monitor.done();
    }
  }

  private void saveCopy( final RrmSimulation rrmSimulation, final INaSimulationData simulationData, final GMLWorkspace simulationWorkspace )
  {
    try
    {
      if( rrmSimulation == null || simulationData == null )
        return;

      final NaModell naModel = simulationData.getNaModel();
      if( naModel != null )
        GmlSerializer.saveWorkspace( naModel.getWorkspace(), rrmSimulation.getModelGml() );

      final NAModellControl naControl = simulationData.getNaControl();
      if( naControl != null )
        GmlSerializer.saveWorkspace( naControl.getWorkspace(), rrmSimulation.getExpertControlGml() );

      if( simulationWorkspace != null )
        GmlSerializer.saveWorkspace( simulationWorkspace, rrmSimulation.getCalculationGml() );
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
    }
  }

  private void saveLog( final NAControl simulation, final RrmSimulation rrmSimulation, final IStatusCollector collector )
  {
    try
    {
      /* Get the log folder and create it, if necessary. */
      final IFolder logFolder = rrmSimulation.getLogFolder();
      final ContainerGenerator generator = new ContainerGenerator( logFolder.getFullPath() );
      generator.generateContainer( null );

      /* Get the calculationStatus.gml. */
      final IFile calculationStatusGml = rrmSimulation.getCalculationStatusGml();

      /* Save the status in the simulation folder. */
      final GeoStatusLog geoStatusLog = new GeoStatusLog( calculationStatusGml );
      geoStatusLog.log( collector.asMultiStatus( String.format( "Calculation of '%s'", simulation.getDescription() ) ) );
      geoStatusLog.serialize();

      /* Refresh. */
      calculationStatusGml.refreshLocal( IResource.DEPTH_ZERO, null );
    }
    catch( final CoreException ex )
    {
      ex.printStackTrace();
    }
  }

  /**
   * This function returns the rrm simulation.
   * 
   * @param simulation
   *          The simulation to calculate.
   */
  private RrmSimulation getRrmSimulation( final NAControl simulation )
  {
    /* Get the description of the simulation. */
    final String description = simulation.getDescription();

    /* Get the folder of the simulation. */
    final IScenarioDataProvider dataProvider = ScenarioHelper.getScenarioDataProvider();
    final IContainer scenarioFolder = dataProvider.getScenarioFolder();
    final IFolder calcCasesFolder = scenarioFolder.getFolder( new Path( INaProjectConstants.FOLDER_RECHENVARIANTEN ) );
    final IFolder simulationFolder = calcCasesFolder.getFolder( description );

    return new RrmSimulation( simulationFolder );
  }
}