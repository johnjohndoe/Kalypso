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
package org.kalypso.ui.rrm.internal.calccase;

import java.util.Locale;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.poi.ss.formula.eval.NotImplementedException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.model.rcm.binding.IMultiGenerator;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * This class executes a catchment model with a multi generator.
 * 
 * @author Holger Albert
 */
public class MultiCatchmentModelRunner extends AbstractCatchmentModelRunner
{
  /**
   * The constructor.
   */
  public MultiCatchmentModelRunner( )
  {
  }

  /**
   * @see org.kalypso.ui.rrm.internal.calccase.AbstractCatchmentModelRunner#executeCatchmentModel(org.kalypso.model.hydrology.project.RrmSimulation,
   *      org.kalypso.model.hydrology.binding.control.NAControl, org.kalypso.model.hydrology.binding.model.NaModell,
   *      org.kalypso.model.rcm.binding.IRainfallGenerator, javax.xml.namespace.QName, java.lang.String,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void executeCatchmentModel( final RrmSimulation simulation, final NAControl control, final NaModell model, final IRainfallGenerator generator, final QName targetLink, final String parameterType, final IProgressMonitor monitor ) throws CoreException
  {
    /* Only IMultiGenerator's are supported. */
    if( !(generator instanceof IMultiGenerator) )
      throw new NotImplementedException( "Only IMultiGenerator's are supported..." ); //$NON-NLS-1$

    /* Cast. */
    final IMultiGenerator multiGenerator = (IMultiGenerator) generator;

    /* Get the sub generators. */
    final IFeatureBindingCollection<IRainfallGenerator> subGenerators = multiGenerator.getSubGenerators();

    try
    {
      /* Monitor. */
      monitor.beginTask( "Apply multi catchment model", (subGenerators.size() * 100) + 300 );
      monitor.subTask( "Validating multi generator..." );

      /* Validate the multi generator. */
      final IStatus validateStatus = CatchmentModelHelper.validateMultiGenerator( multiGenerator, control );
      if( !validateStatus.isOK() )
        throw new CoreException( validateStatus );

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( String.format( "Generate timeseries with %d catchment models...", subGenerators.size() ) );

      /* Hash for catchment to timeseries links. */
      /* HINT: One catchment can contain more than one timeseries link. */
      /* HINT: There should be the same number of timeseries links per catchment in the end, than there are generators. */
      /* HINT: The order of the timeseries links (per catchment) should be same, than the order of the generators. */
      final CatchmentTimeseriesHash hash = new CatchmentTimeseriesHash();

      /* Run all contained generators. */
      for( int i = 0; i < subGenerators.size(); i++ )
        runGenerator( String.format( Locale.PRC, "%d", i ), simulation, control, model, (ILinearSumGenerator) subGenerators.get( i ), targetLink, parameterType, hash, new SubProgressMonitor( monitor, 100 ) );

      /* Monitor. */
      monitor.subTask( "Merge observations for each catchment... " );

      /* The timeseries must be merged. */
      final IFolder simulationFolder = simulation.getSimulationFolder();
      final IPath location = simulationFolder.getLocation();
      final Map<String, IObservation> mergedObservations = hash.merge( location.toFile().toURI().toURL() );

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( "Modify model.gml and save the results..." );

      /* The model.gml links needs to be adjusted. */
      adjustSimulationModelGml( simulation, targetLink, parameterType, mergedObservations );

      /* Monitor. */
      monitor.worked( 100 );
    }
    catch( final Exception ex )
    {
      throw new CoreException( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to execute multi catchment model", ex ) );
    }
    finally
    {
      /* Monitor. */
      monitor.done();

      /* Refresh the simulation folder. */
      refresh( simulation );
    }
  }

  /**
   * This function runs the linear sum generator.
   * 
   * @param prefix
   *          This prefix is used when writing the timeseries.
   * @param simulation
   *          The simulation.
   * @param control
   *          The na control.
   * @param model
   *          The na model.
   * @param generator
   *          The rainfall generator.
   * @param targetLink
   *          The target link.
   * @param parameterType
   *          The parameter type.
   * @param hash
   *          The hash.
   * @param monitor
   *          A progress monitor.
   */
  private void runGenerator( final String prefix, final RrmSimulation simulation, final NAControl control, final NaModell model, final ILinearSumGenerator generator, final QName targetLink, final String parameterType, final CatchmentTimeseriesHash hash, final IProgressMonitor monitor ) throws Exception
  {
    try
    {
      /* Monitor. */
      monitor.beginTask( String.format( "Executing catchment model '%s'...", generator.getDescription() ), 1000 );
      monitor.subTask( "Executing generator..." );

      /* Create the linear sum catchment model runner. */
      final LinearSumCatchmentModelRunner runner = new LinearSumCatchmentModelRunner( prefix );

      /* Calculate the catchment model. */
      runner.executeCatchmentModel( simulation, control, model, generator, targetLink, parameterType, new SubProgressMonitor( monitor, 500 ) );

      /* Load the model.gml of the simulation. */
      final NaModell simulationModel = loadSimulationModelGml( simulation );

      /* Get the catchments. */
      final IFeatureBindingCollection<Catchment> catchments = simulationModel.getCatchments();
      for( final Catchment catchment : catchments )
      {
        /* The feature id of the catchment must be unique. */
        final String id = catchment.getId();

        /* Get the link of the timeseries. */
        TimeseriesLinkType link = null;
        if( parameterType.equals( ITimeseriesConstants.TYPE_RAINFALL ) )
          link = catchment.getPrecipitationLink();
        else if( parameterType.equals( ITimeseriesConstants.TYPE_MEAN_EVAPORATION ) )
          link = catchment.getEvaporationLink();
        else if( parameterType.equals( ITimeseriesConstants.TYPE_MEAN_TEMPERATURE ) )
          link = catchment.getTemperatureLink();
        else
          throw new IllegalArgumentException( "Wrong parameter type." );

        /* Store the timeseries link. */
        hash.put( id, link );
      }

      /* Dispose the workspace again. */
      simulationModel.getWorkspace().dispose();

      /* Monitor. */
      monitor.worked( 500 );
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  private void adjustSimulationModelGml( final RrmSimulation simulation, final QName targetLink, final String parameterType, final Map<String, IObservation> observations ) throws Exception
  {
    /* Load the model.gml of the simulation. */
    final NaModell simulationModel = loadSimulationModelGml( simulation );

    /* Get the catchments. */
    final IFeatureBindingCollection<Catchment> catchments = simulationModel.getCatchments();
    for( final Catchment catchment : catchments )
    {
      /* Get the feature id. */
      final String id = catchment.getId();

      /* Get the observation for that catchment id. */
      final IObservation observation = observations.get( id );

      /* Get the target link. */
      // TODO Optimize, so that equal observations are only saved once...
      final String link = CatchmentModelHelper.buildLink( null, parameterType, catchment );

      /* Set the link. */
      CatchmentModelHelper.setLink( catchment, targetLink, link );

      /* Save the observation. */
      final IFolder simulationFolder = simulation.getSimulationFolder();
      final IFile observationFile = simulationFolder.getFile( new Path( link ) );
      ZmlFactory.writeToFile( observation, observationFile );
    }

    /* Save the model.gml of the simulation. */
    saveModelGml( simulation, simulationModel );

    /* Dispose the workspace. */
    simulationModel.getWorkspace().dispose();
  }

  /**
   * This function loads the model.gml of the simulation.
   * 
   * @param simulation
   *          The simulation.
   * @return The model of the simulation.
   */
  private NaModell loadSimulationModelGml( final RrmSimulation simulation ) throws Exception
  {
    /* Get the file of the model.gml in the simulation. */
    final IFile modelFile = simulation.getModelGml();

    /* Get the workspace. */
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( modelFile );

    /* Get the model. */
    final NaModell simulationModel = (NaModell) workspace.getRootFeature();

    return simulationModel;
  }

  /**
   * This function saves the model.gml of the simulation.
   * 
   * @param simulation
   *          The simulation.
   * @param simulationModel
   *          The model of the simulation.
   */
  private void saveModelGml( final RrmSimulation simulation, final NaModell simulationModel ) throws Exception
  {
    /* Get the file of the model.gml in the simulation. */
    final IFile modelFile = simulation.getModelGml();

    /* Save the model.gml, of the simulation. */
    GmlSerializer.saveWorkspace( simulationModel.getWorkspace(), modelFile );
  }
}