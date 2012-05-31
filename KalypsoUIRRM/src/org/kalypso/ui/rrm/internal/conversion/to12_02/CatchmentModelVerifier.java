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
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
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
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.calccase.CatchmentModelHelper;
import org.kalypso.ui.rrm.internal.simulations.worker.CalculateCatchmentModelsWorker;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * This class verifies catchment models by copying its simultion and calculates them. After that it compares the
 * resulting timeseries.
 * 
 * @author Holger Albert
 */
public class CatchmentModelVerifier
{
  /**
   * The global conversion data.
   */
  private final GlobalConversionData m_globalData;

  /**
   * The simulation with the catchment models to verify.
   */
  private final NAControl m_simulation;

  /**
   * The base folder of the simulations.
   */
  private final File m_baseFolder;

  /**
   * The constructor.
   * 
   * @param globalData
   *          The global conversion data.
   * @param simulation
   *          The simulation with the catchment models to verify.
   * @param baseFolder
   *          The base folder of the simulations.
   */
  public CatchmentModelVerifier( final GlobalConversionData globalData, final NAControl simulation, final File baseFolder )
  {
    m_globalData = globalData;
    m_simulation = simulation;
    m_baseFolder = baseFolder;
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
      final File simulationDir = new File( m_baseFolder, m_simulation.getDescription() );

      /* Create the file handle to the directory of the temporary simulation. */
      simulationTmpDir = new File( m_baseFolder, String.format( "tmp_%s", m_simulation.getDescription() ) );

      /* Create the temporary simulation. */
      createTemporarySimulation( simulationDir, simulationTmpDir );

      /* Create the IFolder. */
      final IContainer[] simulationContainer = ResourcesPlugin.getWorkspace().getRoot().findContainersForLocationURI( simulationDir.toURI() );
      final IFolder simulationFolder = (IFolder) simulationContainer[0];

      /* Create the IFolder. */
      final IContainer[] simulationTmpContainer = ResourcesPlugin.getWorkspace().getRoot().findContainersForLocationURI( simulationTmpDir.toURI() );
      final IFolder simulationTmpFolder = (IFolder) simulationTmpContainer[0];

      /* Calculate the catchment models. */
      final IStatus calculateStatus = calculateCatchmentModels( simulationTmpFolder );
      collector.add( calculateStatus );

      /* Compare the resulting timeseries with the existing ones. */
      final IStatus compareStatus = CatchmentModelHelper.compareTimeseries( simulationFolder, simulationTmpFolder );
      collector.add( compareStatus );

      return collector.asMultiStatus( String.format( "Verify the catchment models of the simulation '%s'", m_simulation.getDescription() ) );
    }
    catch( final Exception ex )
    {
      /* Handle the error as warning. */
      ex.printStackTrace();
      collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex ) );
      return collector.asMultiStatus( String.format( "Verify the catchment models of the simulation '%s'", m_simulation.getDescription() ) );
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
  private IStatus checkGenerators( )
  {
    /* The status collector. */
    final IStatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    final ICatchmentModel catchmentModel = m_globalData.getCatchmentModel();
    final IFeatureBindingCollection<IRainfallGenerator> generators = catchmentModel.getGenerators();

    final String generatorIdN = ((IXLinkedFeature) m_simulation.getProperty( NAControl.PROP_GENERATOR_N )).getFeatureId();
    final String generatorIdE = ((IXLinkedFeature) m_simulation.getProperty( NAControl.PROP_GENERATOR_E )).getFeatureId();
    final String generatorIdT = ((IXLinkedFeature) m_simulation.getProperty( NAControl.PROP_GENERATOR_T )).getFeatureId();

    if( !checkGenerator( generators, generatorIdN ) )
      collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), "The N generator was null or had no factors." ) );
    else
      collector.add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "The N generator was okay." ) );

    if( !checkGenerator( generators, generatorIdE ) )
      collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), "The E generator was null or had no factors." ) );
    else
      collector.add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "The E generator was okay." ) );

    if( !checkGenerator( generators, generatorIdT ) )
      collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), "The T generator was null or had no factors." ) );
    else
      collector.add( new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "The T generator was okay." ) );

    return collector.asMultiStatus( "Checking the N/E/T generators" );
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
    final INaSimulationData simulationData = NaSimulationDataFactory.load( modelURL, null, null, null, null, null, null, null, catchmentModelsUrl, timeseriesMappingsUrl, null, null );

    /* Clone the simulation. */
    final GMLWorkspace simulationWorkspace = FeatureFactory.createGMLWorkspace( m_simulation.getFeatureType(), modelURL, simulationData.getFeatureProviderFactory() );
    final NAControl simulationFeature = (NAControl) simulationWorkspace.getRootFeature();
    FeatureHelper.copyData( m_simulation, simulationFeature );

    /* Set the meta control to the simulation data. */
    simulationData.setMetaControl( simulationFeature );

    final CalculateCatchmentModelsWorker catchmentModelsWorker = new CalculateCatchmentModelsWorker( rrmSimulation, true, simulationData );
    final IStatus status = catchmentModelsWorker.execute( new NullProgressMonitor() );
    if( !status.isOK() )
      return status;

    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "Catchment model generation successfully tested." );
  }
}