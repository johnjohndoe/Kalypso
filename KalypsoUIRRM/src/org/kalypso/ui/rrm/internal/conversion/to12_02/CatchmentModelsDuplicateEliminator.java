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

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.ObjectUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.cm.ICatchmentModel;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.control.SimulationCollection;
import org.kalypso.model.hydrology.project.INaCalcCaseConstants;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.calccase.CatchmentModelHelper;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * This class checks all catchment models and eliminates the ones, that are duplicates. It also corrects the links in
 * the simulation.gml, calculation.gml.
 * 
 * @author Holger Albert
 */
public class CatchmentModelsDuplicateEliminator
{
  /**
   * The converter data (of the current scenario).
   */
  private final ConverterData m_data;

  /**
   * The constructor.
   * 
   * @param data
   *          The converter data (of the current scenario).
   */
  public CatchmentModelsDuplicateEliminator( final ConverterData data )
  {
    m_data = data;
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

    try
    {
      /* All generators. */
      final ICatchmentModel catchmentModel = m_data.loadModel( INaProjectConstants.GML_CATCHMENT_MODEL_PATH );
      final IFeatureBindingCollection<IRainfallGenerator> allGenerators = catchmentModel.getGenerators();

      /* Used generators. */
      final List<ILinearSumGenerator> usedGenerators = new ArrayList<ILinearSumGenerator>();

      /* Verify each generator. */
      final IRainfallGenerator[] generators = allGenerators.toArray( new IRainfallGenerator[] {} );
      for( final IRainfallGenerator generator : generators )
      {
        final IStatus status = verifyGenerator( allGenerators, usedGenerators, generator );
        collector.add( status );
      }

      /* Save the catchments models. */
      m_data.saveModel( INaProjectConstants.GML_CATCHMENT_MODEL_PATH, catchmentModel );

      return collector.asMultiStatusOrOK( Messages.getString( "CatchmentModelsDuplicateEliminator.0" ) ); //$NON-NLS-1$
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
      collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex ) );
      return collector.asMultiStatusOrOK( Messages.getString( "CatchmentModelsDuplicateEliminator.1" ) ); //$NON-NLS-1$
    }
  }

  private IStatus verifyGenerator( final IFeatureBindingCollection<IRainfallGenerator> allGenerators, final List<ILinearSumGenerator> usedGenerators, final IRainfallGenerator generator ) throws Exception
  {
    /* The status collector. */
    final IStatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    /* The description of the generator equals the name of the calculation case it belongs to. */
    final String description = generator.getDescription();

    /* Checks, if an equal generator is used. */
    final ILinearSumGenerator usedGenerator = isGeneratorUsed( usedGenerators, (ILinearSumGenerator) generator );
    if( usedGenerator != null )
    {
      /* The description of the generator equals the name of the calculation case it belongs to. */
      final String usedDescription = usedGenerator.getDescription();

      /* Adjust the logs. */
      collector.add( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "CatchmentModelsDuplicateEliminator.2" ), description, generator.getParameterType(), usedDescription, TimeseriesUtils.getName( usedGenerator.getParameterType() ) ) ) ); //$NON-NLS-1$

      /* Each generator, for which an equal one exists in the list of used generators */
      /* will be removed from the original list of generators. */
      allGenerators.remove( generator );

      /* Adjust the logs. */
      collector.add( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "CatchmentModelsDuplicateEliminator.3" ) ) ) ); //$NON-NLS-1$

      /* The Basis/.models/simulations.gml and the Basis/[CalcCase]/.models/calculation.gml must be adjusted. */
      adjustDataFiles( usedGenerator, description );

      /* Adjust the logs. */
      collector.add( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "CatchmentModelsDuplicateEliminator.4" ), usedDescription, TimeseriesUtils.getName( usedGenerator.getParameterType() ) ) ) ); //$NON-NLS-1$

      /* The comment of the one, which is used must be altered to reflect all CCs which use it now. */
      final String usedComment = usedGenerator.getComment();
      String newComment = null;
      if( usedComment == null || usedComment.length() == 0 || usedComment.equals( Messages.getString( Messages.getString( "CatchmentModelBuilder_0" ) ) ) ) //$NON-NLS-1$
        newComment = String.format( Messages.getString( "CatchmentModelsDuplicateEliminator.6" ), usedDescription, description ); //$NON-NLS-1$
      else
        newComment = String.format( "%s, %s", usedComment, description ); //$NON-NLS-1$
      usedGenerator.setComment( newComment );

      return collector.asMultiStatus( String.format( Messages.getString( "CatchmentModelsDuplicateEliminator.8" ), description, TimeseriesUtils.getName( generator.getParameterType() ) ) ); //$NON-NLS-1$
    }

    /* Adjust the logs. */
    collector.add( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), String.format( Messages.getString( "CatchmentModelsDuplicateEliminator.9" ), description, TimeseriesUtils.getName( generator.getParameterType() ) ) ) ); //$NON-NLS-1$

    /* Each generator for which no equal one exists will be added to the list of used generators. */
    /* Nothing else must be done. */
    usedGenerators.add( (ILinearSumGenerator) generator );

    return collector.asMultiStatus( String.format( Messages.getString( "CatchmentModelsDuplicateEliminator.10" ), description, TimeseriesUtils.getName( generator.getParameterType() ) ) ); //$NON-NLS-1$
  }

  private ILinearSumGenerator isGeneratorUsed( final List<ILinearSumGenerator> usedGenerators, final ILinearSumGenerator generator )
  {
    /* Compare the generator to the used generators. */
    for( final ILinearSumGenerator usedGenerator : usedGenerators )
    {
      if( compareGenerator( usedGenerator, generator ) )
        return usedGenerator;
    }

    return null;
  }

  private void adjustDataFiles( final ILinearSumGenerator usedGenerator, final String generatorName ) throws Exception
  {
    /* Adjust the global simulations.gml. */
    adjustSimulationsGml( usedGenerator, generatorName );

    /* Adjust the CC calculation.gml. */
    adjustCalculationGml( usedGenerator, generatorName );
  }

  private void adjustSimulationsGml( final ILinearSumGenerator usedGenerator, final String generatorName ) throws Exception
  {
    /* Adjust the corresponding simulation. */
    final SimulationCollection simulations = m_data.loadModel( INaProjectConstants.GML_SIMULATIONS_PATH );
    for( final NAControl simulation : simulations.getSimulations() )
    {
      final String simulationName = simulation.getDescription();
      if( simulationName.equals( generatorName ) )
      {
        /* HINT: The global data is saved afterwards. */
        adjustNaControl( usedGenerator, simulation );
        return;
      }
    }

    /* Save the simulations */
    m_data.saveModel( INaProjectConstants.GML_SIMULATIONS_PATH, simulations );
  }

  private void adjustCalculationGml( final ILinearSumGenerator usedGenerator, final String generatorName ) throws Exception
  {
    /* Create the file handle to the calculation.gml file. */
    final IPath basePath = Path.fromOSString( m_data.getBaseDir().getAbsolutePath() );
    final IPath simulationsPath = basePath.append( RrmScenario.FOLDER_SIMULATIONEN );
    final IPath simulationPath = simulationsPath.append( generatorName );
    final IPath modelsPath = simulationPath.append( INaProjectConstants.FOLDER_MODELS );
    final IPath calculationGmlPath = modelsPath.append( INaCalcCaseConstants.CALCULATION_GML_FILE );
    final IFile[] calculationGmlFiles = ResourcesPlugin.getWorkspace().getRoot().findFilesForLocationURI( calculationGmlPath.toFile().toURI() );
    final IFile calculationGmlFile = calculationGmlFiles[0];

    /* Load the calculation.gml. */
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( calculationGmlFile );
    final NAControl calculation = (NAControl) workspace.getRootFeature();

    /* HINT: We need to save the calculation.gml. */
    adjustNaControl( usedGenerator, calculation );

    /* Save the calculation gml. */
    GmlSerializer.saveWorkspace( workspace, calculationGmlFile );

    /* Dispose the workspace. */
    workspace.dispose();
  }

  private void adjustNaControl( final ILinearSumGenerator usedGenerator, final NAControl control )
  {
    /* Build the href. */
    final String usedHref = String.format( "%s#%s", INaProjectConstants.GML_CATCHMENT_MODEL_FILE, usedGenerator.getId() ); //$NON-NLS-1$

    /* Adjust the generator reference for parameter type N, if needed. */
    if( usedGenerator.getParameterType().equals( ITimeseriesConstants.TYPE_RAINFALL ) )
      control.setGeneratorReferenceN( usedHref );

    /* Adjust the generator reference for parameter type E, if needed. */
    if( usedGenerator.getParameterType().equals( ITimeseriesConstants.TYPE_EVAPORATION_LAND_BASED ) )
      control.setGeneratorReferenceE( usedHref );

    /* Adjust the generator reference for parameter type T, if needed. */
    if( usedGenerator.getParameterType().equals( ITimeseriesConstants.TYPE_MEAN_TEMPERATURE ) )
      control.setGeneratorReferenceT( usedHref );
  }

  public static boolean compareGenerator( final ILinearSumGenerator usedGenerator, final ILinearSumGenerator generator )
  {
    /* Check the parameter type. */
    if( !ObjectUtils.equals( usedGenerator.getParameterType(), generator.getParameterType() ) )
      return false;

    /* Check the timestep. */
    if( !ObjectUtils.equals( usedGenerator.getTimestep(), generator.getTimestep() ) )
      return false;

    return CatchmentModelHelper.compareGeneratorCatchments( usedGenerator, generator, true );
  }
}