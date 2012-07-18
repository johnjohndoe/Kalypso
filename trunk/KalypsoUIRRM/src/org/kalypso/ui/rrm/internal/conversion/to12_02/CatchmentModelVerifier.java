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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.io.File;
import java.io.IOException;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.IStatusWithTime;
import org.kalypso.contribs.eclipse.core.runtime.MultiStatusWithTime;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusWithTime;
import org.kalypso.model.hydrology.INaSimulationData;
import org.kalypso.model.hydrology.NaSimulationDataFactory;
import org.kalypso.model.hydrology.binding.cm.ICatchment;
import org.kalypso.model.hydrology.binding.cm.ICatchmentModel;
import org.kalypso.model.hydrology.binding.cm.IFactorizedTimeseries;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.calccase.CatchmentModelHelper;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.simulations.worker.CalculateCatchmentModelsWorker;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IXLinkedFeature;

/**
 * This class verifies catchment models by copying its simultion and calculates them. After that it compares the
 * resulting timeseries.
 * 
 * @author Holger Albert
 */
public class CatchmentModelVerifier
{
  /**
   * True, if there is design rainfall.
   */
  private final boolean m_hasSynth;

  /**
   * The converter data.
   */
  private final ConverterData m_data;

  /**
   * The simulation with the catchment models to verify.
   */
  private final NAControl m_simulation;

  /**
   * The base folder of the simulations.
   */
  private final File m_simulationsFolder;

  /**
   * The constructor.
   * 
   * @param hasSynth
   *          True, if there is design rainfall.
   * @param data
   *          The converter data.
   * @param simulation
   *          The simulation with the catchment models to verify.
   * @param simulationsFolder
   *          The base folder of the simulations.
   */
  public CatchmentModelVerifier( final boolean hasSynth, final ConverterData data, final NAControl simulation, final File simulationsFolder )
  {
    m_hasSynth = hasSynth;
    m_data = data;
    m_simulation = simulation;
    m_simulationsFolder = simulationsFolder;
  }

  /**
   * This function executes the operation.
   * 
   * @return A status object, indicating the result of the operation.
   */
  public IStatus execute( )
  {
    /* The status collector. */
    final IStatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    /* The directory of the temporary simulation. */
    File simulationTmpDir = null;

    try
    {
      /* Check the generators. */
      final IStatus status = checkGenerators();
      collector.add( status );

      /* Create the file handle to the directory of the simulation. */
      final File simulationDir = new File( m_simulationsFolder, m_simulation.getDescription() );

      /* Create the file handle to the directory of the temporary simulation. */
      simulationTmpDir = new File( m_simulationsFolder, String.format( "tmp_%s", m_simulation.getDescription() ) ); //$NON-NLS-1$

      /* Create the temporary simulation. */
      createTemporarySimulation( simulationDir, simulationTmpDir );

      /* Create the IFolder. */
      final IContainer[] simulationContainer = ResourcesPlugin.getWorkspace().getRoot().findContainersForLocationURI( simulationDir.toURI() );
      final IFolder simulationFolder = (IFolder) simulationContainer[0];

      /* Create the IFolder. */
      final IContainer[] simulationTmpContainer = ResourcesPlugin.getWorkspace().getRoot().findContainersForLocationURI( simulationTmpDir.toURI() );
      final IFolder simulationTmpFolder = (IFolder) simulationTmpContainer[0];

      /* Create the IFolder. */
      final IContainer[] scenarioContainer = ResourcesPlugin.getWorkspace().getRoot().findContainersForLocationURI( m_data.getBaseDir().toURI() );
      final IFolder scenarioFolder = (IFolder) scenarioContainer[0];
      scenarioFolder.refreshLocal( IResource.DEPTH_INFINITE, null );

      /* Calculate the catchment models. */
      final IStatus calculateStatus = calculateCatchmentModels( simulationTmpFolder );
      collector.add( calculateStatus );

      /* Compare the resulting timeseries with the existing ones. */
      final IStatus compareStatus = CatchmentModelHelper.compareTimeseries( simulationFolder, simulationTmpFolder );
      collector.add( compareStatus );

      return collector.asMultiStatus( String.format( Messages.getString( "CatchmentModelVerifier_1" ), m_simulation.getDescription() ) ); //$NON-NLS-1$
    }
    catch( final Exception ex )
    {
      /* Handle the error as warning. */
      ex.printStackTrace();
      collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex ) );
      return collector.asMultiStatus( String.format( Messages.getString( "CatchmentModelVerifier_2" ), m_simulation.getDescription() ) ); //$NON-NLS-1$
    }
    finally
    {
      /* Delete the temporary simulation. */
      if( simulationTmpDir != null )
        FileUtils.deleteQuietly( simulationTmpDir );
    }
  }

  /**
   * This function copies the simulation to a temporary one.
   * 
   * @param simulationDir
   *          The directory of the simulation.
   */
  private void createTemporarySimulation( final File simulationDir, final File simulationTmpDir ) throws IOException
  {
    /* Copy the existing simulation. */
    FileUtils.copyDirectory( simulationDir, simulationTmpDir );
  }

  /**
   * This function verifies the generators.
   * 
   * @return A WARNING status if generators are missing or if they have no factorized timeseries (hence no factors).
   *         Then no comparison makes sense. Otherwise a OK Status is returned.
   */
  private IStatus checkGenerators( ) throws Exception
  {
    final IStatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    if( m_hasSynth )
    {
      collector.add( IStatus.OK, Messages.getString("CatchmentModelVerifier.0") ); //$NON-NLS-1$
      return collector.asMultiStatus( Messages.getString( "CatchmentModelVerifier_9" ) ); //$NON-NLS-1$
    }

    final String catchmentModelPath = RrmScenario.FOLDER_MODELS + '/' + RrmScenario.FILE_CATCHMENT_MODELS_GML;

    final ICatchmentModel catchmentModel = m_data.loadModel( catchmentModelPath );
    final IFeatureBindingCollection<IRainfallGenerator> generators = catchmentModel.getGenerators();

    final String generatorIdN = ((IXLinkedFeature) m_simulation.getProperty( NAControl.PROP_GENERATOR_N )).getFeatureId();
    final String generatorIdE = ((IXLinkedFeature) m_simulation.getProperty( NAControl.PROP_GENERATOR_E )).getFeatureId();
    final String generatorIdT = ((IXLinkedFeature) m_simulation.getProperty( NAControl.PROP_GENERATOR_T )).getFeatureId();

    if( !checkGenerator( generators, generatorIdN ) )
      collector.add( IStatus.WARNING, Messages.getString( "CatchmentModelVerifier_3" ) ); //$NON-NLS-1$
    else
      collector.add( IStatus.OK, Messages.getString( "CatchmentModelVerifier_4" ) ); //$NON-NLS-1$

    if( !checkGenerator( generators, generatorIdE ) )
      collector.add( IStatus.WARNING, Messages.getString( "CatchmentModelVerifier_5" ) ); //$NON-NLS-1$
    else
      collector.add( IStatus.OK, Messages.getString( "CatchmentModelVerifier_6" ) ); //$NON-NLS-1$

    if( !checkGenerator( generators, generatorIdT ) )
      collector.add( IStatus.WARNING, Messages.getString( "CatchmentModelVerifier_7" ) ); //$NON-NLS-1$
    else
      collector.add( IStatus.OK, Messages.getString( "CatchmentModelVerifier_8" ) ); //$NON-NLS-1$

    return collector.asMultiStatus( Messages.getString( "CatchmentModelVerifier_9" ) ); //$NON-NLS-1$
  }

  /**
   * This function verifies a generator.
   * 
   * @param generators
   *          The generators.
   * @param generatorId
   *          The id of the generator.
   * @return False if the generator is missing or if it has no factorized timeseries (hence no factors). Then no
   *         comparison makes sense. Otherwise true is returned.
   */
  private boolean checkGenerator( final IFeatureBindingCollection<IRainfallGenerator> generators, final String generatorId )
  {
    /* Find the generator. */
    final IRainfallGenerator generator = findGenerator( generators, generatorId );
    if( generator == null )
      return false;

    /* The timestep is only defined in linear sum generators for now. */
    if( !(generator instanceof ILinearSumGenerator) )
      return false;

    /* Cast. */
    final ILinearSumGenerator linearGenerator = (ILinearSumGenerator) generator;

    /* Get the catchments. */
    final IFeatureBindingCollection<ICatchment> catchments = linearGenerator.getCatchments();
    if( catchments == null || catchments.size() == 0 )
      return false;

    /* Check the factorized timeseries of each catchment. */
    for( final ICatchment catchment : catchments )
    {
      final IFeatureBindingCollection<IFactorizedTimeseries> factorizedTimeseries = catchment.getFactorizedTimeseries();
      if( factorizedTimeseries == null || factorizedTimeseries.size() == 0 )
        return false;
    }

    return true;
  }

  /**
   * This function returns the generator with the given generator id.
   * 
   * @param generators
   *          All generators.
   * @param generatorId
   *          The id of the generator.
   * @return The generator with the given generator id or null, if it was not found.
   */
  private IRainfallGenerator findGenerator( final IFeatureBindingCollection<IRainfallGenerator> generators, final String generatorId )
  {
    for( final IRainfallGenerator generator : generators )
    {
      final String id = generator.getId();
      if( id.equals( generatorId ) )
        return generator;
    }

    return null;
  }

  /**
   * This function calculates the catchment models.
   * 
   * @param simulationTmpFolder
   *          The folder of the temporary simulation.
   * @return A status indicating the results of the operation.
   */
  private IStatus calculateCatchmentModels( final IFolder simulationTmpFolder ) throws Exception
  {
    /* Create the rrm simulation and get the rrm scenario. */
    final RrmSimulation rrmSimulation = new RrmSimulation( simulationTmpFolder );
    final RrmScenario rrmScenario = rrmSimulation.getScenario();

    /* Create the URLs. */
    final URL modelURL = ResourceUtilities.createURL( rrmScenario.getModelFile() );
    final URL catchmentModelsUrl = ResourceUtilities.createURL( rrmScenario.getCatchmentModelsGml() );
    final URL timeseriesMappingsUrl = ResourceUtilities.createURL( rrmScenario.getTimeseriesMappingsGml() );

    /* Load all simulation data. */
    final INaSimulationData simulationData = NaSimulationDataFactory.load( modelURL, null, null, null, null, null, null, catchmentModelsUrl, timeseriesMappingsUrl, null, null );

    /* Load the simulation. */
    final GMLWorkspace simulationWorkspace = GmlSerializer.createGMLWorkspace( rrmSimulation.getCalculationGml(), modelURL, simulationData.getFeatureProviderFactory(), null );
    final NAControl simulationFeature = (NAControl) simulationWorkspace.getRootFeature();

    /* Set the meta control to the simulation data. */
    simulationData.setMetaControl( simulationFeature );

    /* Calculate the catchment models. */
    final CalculateCatchmentModelsWorker catchmentModelsWorker = new CalculateCatchmentModelsWorker( rrmSimulation, true, simulationData );
    final IStatus status = catchmentModelsWorker.execute( new NullProgressMonitor() );
    if( !status.isOK() )
      return convertStatus( status );

    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString( "CatchmentModelVerifier_10" ) ); //$NON-NLS-1$
  }

  private IStatus convertStatus( final IStatus status )
  {
    if( status instanceof MultiStatus )
      return convertMultiStatus( (MultiStatus) status );

    return convertSingleStatus( status );
  }

  private IStatus convertMultiStatus( final MultiStatus multiStatus )
  {
    MultiStatus resultMultiStatus = null;
    if( multiStatus instanceof IStatusWithTime )
      resultMultiStatus = new MultiStatusWithTime( KalypsoUIRRMPlugin.getID(), multiStatus.getCode(), multiStatus.getMessage(), ((IStatusWithTime) multiStatus).getTime(), multiStatus.getException() );
    else
      resultMultiStatus = new MultiStatus( KalypsoUIRRMPlugin.getID(), multiStatus.getCode(), multiStatus.getMessage(), multiStatus.getException() );

    final IStatus[] children = multiStatus.getChildren();
    for( final IStatus status : children )
    {
      final IStatus resultStatus = convertStatus( status );
      resultMultiStatus.add( resultStatus );
    }

    return resultMultiStatus;
  }

  private IStatus convertSingleStatus( final IStatus status )
  {
    int severity = status.getSeverity();
    if( severity >= IStatus.ERROR )
      severity = IStatus.WARNING;

    final String plugin = status.getPlugin();
    final String message = status.getMessage();
    final Throwable exception = status.getException();

    if( status instanceof IStatusWithTime )
      return new StatusWithTime( severity, plugin, message, ((IStatusWithTime) status).getTime(), exception );

    return new Status( severity, plugin, message, exception );
  }
}