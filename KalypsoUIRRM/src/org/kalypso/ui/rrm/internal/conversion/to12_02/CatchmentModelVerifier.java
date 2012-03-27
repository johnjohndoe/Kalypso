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

import org.apache.commons.io.FileUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.cm.binding.ICatchmentModel;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.model.rcm.binding.ICatchment;
import org.kalypso.model.rcm.binding.IFactorizedTimeseries;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.calccase.UpdateSimulationWorker;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

import com.google.common.base.Charsets;

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
    final StatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    /* The directory of the temporary simulation. */
    File simulationTmpDir = null;

    try
    {
      /* Create the temporary simulation. */
      simulationTmpDir = createTemporarySimulation();

      /* Create the IFolder. */
      final IContainer[] simulationTmpContainer = ResourcesPlugin.getWorkspace().getRoot().findContainersForLocationURI( simulationTmpDir.toURI() );
      final IFolder simulationTmpFolder = (IFolder) simulationTmpContainer[0];

      /* Check the generators. */
// final IStatus status = checkGenerators();
// collector.add( status );
// if( !status.isOK() )
// {
// collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(),
// "Cannot calculate the catchment models to compare the timeseries..." ) );
// return collector.asMultiStatus( String.format( "Verify the catchment models of the simulation '%s'...",
// m_simulation.getDescription() ) );
// }

      /* Calculate the catchment models. */
      final IStatus calculateStatus = calculateCatchmentModels( simulationTmpFolder );
      collector.add( calculateStatus );

      /* Compare the resulting timeseries with the existing ones. */
      compareResultingTimeseries();

      return collector.asMultiStatus( String.format( "Verify the catchment models of the simulation '%s'...", m_simulation.getDescription() ) );
    }
    catch( final Exception ex )
    {
      /* Handle the error as warning. */
      ex.printStackTrace();
      collector.add( new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex ) );
      return collector.asMultiStatus( String.format( "Verify the catchment models of the simulation '%s'...", m_simulation.getDescription() ) );
    }
    finally
    {
      /* Delete the temporary simulation. */
      if( simulationTmpDir != null )
        FileUtils.deleteQuietly( simulationTmpDir );
    }
  }

  /**
   * This function copies the simulation to a temporary one and saves the data to it, which is needed to calculate the
   * catchment models.
   * 
   * @return The directory of the temporary simulation.
   */
  private File createTemporarySimulation( ) throws IOException, GmlSerializeException
  {
    /* Create the file handle to the directory of the simulation. */
    final File simulationDir = new File( m_baseFolder, m_simulation.getDescription() );

    /* Create the file handle to the directory of the temporary simulation. */
    final File simulationTmpDir = new File( m_baseFolder, String.format( "tmp_%s", m_simulation.getDescription() ) );

    /* Copy the existing simulation. */
    FileUtils.copyDirectory( simulationDir, simulationTmpDir );

    /* Save the catchmentModels.gml into the temporary simulation. */
    // FIXME If the .models folder in a calc case is emptied, this file needs to be saved to the Basis/.models folder.
    // FIXME At the moment the file in the calc case is used to calculate the catchment models.
    final File catchmentModelsTmpFile = new File( simulationTmpDir, INaProjectConstants.GML_CATCHMENT_MODEL_PATH );
    final ICatchmentModel catchmentModel = m_globalData.getCatchmentModel();
    GmlSerializer.serializeWorkspace( catchmentModelsTmpFile, catchmentModel.getWorkspace(), Charsets.UTF_8.name() );

    return simulationTmpDir;
  }

  /**
   * This function verifies the generators.
   * 
   * @return A WARNING status if generators are missing or if they have no factorized timeseries (hence no factors).
   *         Then no comparison makes sense. Otherwise a OK Status is returned.
   */
  private IStatus checkGenerators( )
  {
    if( !checkGenerator( m_simulation.getGeneratorN() ) )
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), "The N generator was null or had no factors..." );

    if( !checkGenerator( m_simulation.getGeneratorE() ) )
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), "The E generator was null or had no factors..." );

    if( !checkGenerator( m_simulation.getGeneratorT() ) )
      return new Status( IStatus.WARNING, KalypsoUIRRMPlugin.getID(), "The T generator was null or had no factors..." );

    return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), "The N/E/T generators were ok." );
  }

  /**
   * This function verifies a generator.
   * 
   * @param generator
   *          The generator.
   * @return A WARNING status if the generator is missing or if it has no factorized timeseries (hence no factors). Then
   *         no comparison makes sense. Otherwise a OK Status is returned.
   */
  private boolean checkGenerator( final IRainfallGenerator generator )
  {
    /* If the generator is null, return false. */
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
   * This function calculates the catchment models.
   * 
   * @param simulationTmpFolder
   *          The folder of the temporary simulation.
   * @return A status indicating the results of the operation.
   */
  private IStatus calculateCatchmentModels( final IFolder simulationTmpFolder ) throws CoreException
  {
    final UpdateSimulationWorker updateWorker = new UpdateSimulationWorker( simulationTmpFolder );

    return updateWorker.execute( new NullProgressMonitor() );
  }

  private void compareResultingTimeseries( )
  {
    // TODO
  }
}