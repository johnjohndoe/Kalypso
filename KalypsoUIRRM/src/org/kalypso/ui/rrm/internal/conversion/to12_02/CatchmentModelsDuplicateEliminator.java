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
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.control.SimulationCollection;
import org.kalypso.model.hydrology.project.INaCalcCaseConstants;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.model.rcm.binding.ICatchment;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.model.rcm.util.RainfallGeneratorUtilities;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IXLinkedFeature;

/**
 * This class checks all catchment models and eliminates the ones, that are duplicates. It also corrects the links in
 * the simulation.gml, calculation.gml.
 * 
 * @author Holger Albert
 */
public class CatchmentModelsDuplicateEliminator
{
  /**
   * The target directory.
   */
  private final File m_targetDir;

  /**
   * The global conversion data.
   */
  private final GlobalConversionData m_globalData;

  /**
   * The constructor.
   * 
   * @param targetDir
   *          The target directory.
   * @param globalData
   *          The global conversion data.
   */
  public CatchmentModelsDuplicateEliminator( final File targetDir, final GlobalConversionData globalData )
  {
    m_targetDir = targetDir;
    m_globalData = globalData;
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

    try
    {
      /* All generators. */
      final IFeatureBindingCollection<IRainfallGenerator> allGenerators = m_globalData.getCatchmentModel().getGenerators();

      /* Used generators. */
      final List<ILinearSumGenerator> usedGenerators = new ArrayList<ILinearSumGenerator>();

      /* Verify each generator. */
      final IRainfallGenerator[] generators = allGenerators.toArray( new IRainfallGenerator[] {} );
      for( final IRainfallGenerator generator : generators )
      {
        final IStatus status = verifyGenerator( allGenerators, usedGenerators, generator );
        collector.add( status );
      }

      /* Save the global models (catchmentModels.gml and simulations.gml). */
      m_globalData.saveGlobalModels( m_targetDir, collector );

      return collector.asMultiStatusOrOK( "Check catchment models for duplicates..." );
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
      collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex ) );
      return collector.asMultiStatusOrOK( "Check catchment models for duplicates..." );
    }
  }

  private IStatus verifyGenerator( final IFeatureBindingCollection<IRainfallGenerator> allGenerators, final List<ILinearSumGenerator> usedGenerators, final IRainfallGenerator generator ) throws Exception
  {
    /* The status collector. */
    final StatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    /* The description of the generator equals the name of the calculation case it belongs to. */
    final String description = generator.getDescription();

    /* Checks, if an equal generator is used. */
    final ILinearSumGenerator usedGenerator = isGeneratorUsed( usedGenerators, (ILinearSumGenerator) generator );
    if( usedGenerator != null )
    {
      /* The description of the generator equals the name of the calculation case it belongs to. */
      final String usedDescription = usedGenerator.getDescription();

      /* Adjust the logs. */
      collector.add( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), String.format( "The generator '%s (%s)' equals the generator '%s (%s)' and was removed.", description, generator.getParameterType(), usedDescription, usedGenerator.getParameterType() ) ) );

      /* Each generator, for which an equal one exists in the list of used generators */
      /* will be removed from the original list of generators. */
      allGenerators.remove( generator );

      /* Adjust the logs. */
      collector.add( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), String.format( "The data files referencing it were adjusted." ) ) );

      /* The Basis/.models/simulations.gml and the Basis/[CalcCase]/.models/calculation.gml must be adjusted. */
      adjustDataFiles( usedGenerator, description );

      /* Adjust the logs. */
      collector.add( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), String.format( "The description of the generator '%s (%s)' was adjusted.", usedDescription, usedGenerator.getParameterType() ) ) );

      /* The description of the one, which is used must be altered to reflect all CCs which use it now. */
      final String newDescription = String.format( "%s, %s", usedDescription, description );
      usedGenerator.setDescription( newDescription );

      return collector.asMultiStatus( String.format( "Generator '%s (%s)'", description, generator.getParameterType() ) );
    }

    /* Adjust the logs. */
    collector.add( new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), String.format( "The generator '%s (%s)' was kept.", description, generator.getParameterType() ) ) );

    /* Each generator for which no equal one exists will be added to the list of used generators. */
    /* Nothing else must be done. */
    usedGenerators.add( (ILinearSumGenerator) generator );

    return collector.asMultiStatus( String.format( "Generator '%s (%s)'", description, generator.getParameterType() ) );
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

    /* Adjust the catchmentModels.gml in the CC? */
    // TODO At the moment it is not uptodate after an project import...
    // TODO A new calculation of the simulation copies the global one to the calculation case...
  }

  private void adjustSimulationsGml( final ILinearSumGenerator usedGenerator, final String generatorName )
  {
    /* Adjust the corresponding simulation. */
    final SimulationCollection simulations = m_globalData.getSimulations();
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
  }

  private void adjustCalculationGml( final ILinearSumGenerator usedGenerator, final String generatorName ) throws Exception
  {
    /* Create the file handle to the calculation.gml file. */
    final IPath targetPath = Path.fromOSString( m_targetDir.getAbsolutePath() );
    final IPath basePath = targetPath.append( INaProjectConstants.FOLDER_BASIS );
    final IPath simulationsPath = basePath.append( INaProjectConstants.FOLDER_RECHENVARIANTEN );
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
    final String usedHref = String.format( "%s#%s", INaProjectConstants.GML_CATCHMENT_MODEL_FILE, usedGenerator.getId() );

    /* Adjust the generator reference for parameter type N, if needed. */
    if( usedGenerator.getParameterType().equals( ITimeseriesConstants.TYPE_RAINFALL ) )
      control.setGeneratorReferenceN( usedHref );

    /* Adjust the generator reference for parameter type E, if needed. */
    if( usedGenerator.getParameterType().equals( ITimeseriesConstants.TYPE_EVAPORATION ) )
      control.setGeneratorReferenceE( usedHref );

    /* Adjust the generator reference for parameter type T, if needed. */
    if( usedGenerator.getParameterType().equals( ITimeseriesConstants.TYPE_TEMPERATURE ) )
      control.setGeneratorReferenceT( usedHref );
  }

  private boolean compareGenerator( final ILinearSumGenerator usedGenerator, final ILinearSumGenerator generator )
  {
    /* Check the parameter type. */
    if( !usedGenerator.getParameterType().equals( generator.getParameterType() ) )
      return false;

    /* Get the catchments. */
    final IFeatureBindingCollection<ICatchment> usedCatchments = usedGenerator.getCatchments();
    final IFeatureBindingCollection<ICatchment> catchments = generator.getCatchments();
    if( usedCatchments.size() != catchments.size() )
      return false;

    /* Compare the catchments. */
    for( int i = 0; i < usedCatchments.size(); i++ )
    {
      /* Get the catchments. */
      final ICatchment usedCatchment = usedCatchments.get( i );
      final ICatchment catchment = catchments.get( i );

      /* If the linked areas do not match, this are completely different lists or not in the same order. */
      final String usedAreaHref = ((IXLinkedFeature) usedCatchment.getProperty( ICatchment.PROPERTY_AREA_LINK )).getHref();
      final String areaHref = ((IXLinkedFeature) catchment.getProperty( ICatchment.PROPERTY_AREA_LINK )).getHref();
      if( !usedAreaHref.equals( areaHref ) )
        return false;

      /* Build the hash. */
      final String usedHash = RainfallGeneratorUtilities.buildHash( usedCatchment );
      final String hash = RainfallGeneratorUtilities.buildHash( catchment );
      if( !usedHash.equals( hash ) )
        return false;
    }

    return true;
  }
}