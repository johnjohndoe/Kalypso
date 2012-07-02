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

import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollectorWithTime;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.cm.ICatchmentModel;
import org.kalypso.model.hydrology.binding.cm.IMultiGenerator;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.control.SimulationCollection;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMapping;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMappingCollection;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

import de.renew.workflow.connector.cases.IScenario;

/**
 * Worker for merging catchment models and timeseries mappings of the source scenarios into the target scenario.
 * 
 * @author Holger Albert
 */
public class MergeMappingsWorker
{
  /**
   * The source scenarios.
   */
  private final IScenario[] m_sourceScenarios;

  /**
   * The target scenario.
   */
  private final IScenario m_targetScenario;

  /**
   * The merge mappings helper.
   */
  private final MergeMappingsHelper m_mappingsHelper;

  /**
   * The constructor.
   * 
   * @param sourceScenarios
   *          The source scenarios.
   * @param targetScenario
   *          The target scenario.
   */
  public MergeMappingsWorker( final IScenario[] sourceScenarios, final IScenario targetScenario )
  {
    m_sourceScenarios = sourceScenarios;
    m_targetScenario = targetScenario;
    m_mappingsHelper = new MergeMappingsHelper();
  }

  public IStatus analyze( IProgressMonitor monitor )
  {
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    /* The status collector. */
    final IStatusCollector collector = new StatusCollectorWithTime( KalypsoUIRRMPlugin.getID() );

    try
    {
      /* Monitor. */
      monitor.beginTask( Messages.getString( "MergeMappingsWorker_0" ), 250 + 250 * m_sourceScenarios.length ); //$NON-NLS-1$
      monitor.subTask( String.format( Messages.getString( "MergeMappingsWorker_1" ), m_targetScenario.getName() ) ); //$NON-NLS-1$

      /* Analyze the target scenario. */
      analyzeGenerators( m_targetScenario, true );
      analyzeMappings( m_targetScenario, true );

      /* Monitor. */
      monitor.worked( 250 );

      /* Loop the source scenarios. */
      for( final IScenario sourceScenario : m_sourceScenarios )
      {
        /* Monitor. */
        monitor.subTask( String.format( Messages.getString( "MergeMappingsWorker_2" ), sourceScenario.getName() ) ); //$NON-NLS-1$

        /* Analyze the source scenario. */
        analyzeGenerators( sourceScenario, false );
        analyzeMappings( sourceScenario, false );

        /* Monitor. */
        monitor.worked( 250 );
      }

      return collector.asMultiStatus( Messages.getString( "MergeMappingsWorker_3" ) ); //$NON-NLS-1$
    }
    catch( final Exception ex )
    {
      collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex ) );
      return collector.asMultiStatus( Messages.getString( "MergeMappingsWorker_4" ) ); //$NON-NLS-1$
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  private void analyzeGenerators( final IScenario scenario, final boolean existing ) throws Exception
  {
    /* The rrm scenario. */
    final RrmScenario rrmScenario = new RrmScenario( scenario.getFolder() );

    /* Load the catchment models. */
    final IResource catchmentModelsGml = rrmScenario.getCatchmentModelsGml();
    final GMLWorkspace catchmentModelsWorkspace = GmlSerializer.createGMLWorkspace( (IFile) catchmentModelsGml );
    final ICatchmentModel catchmentModel = (ICatchmentModel) catchmentModelsWorkspace.getRootFeature();
    final IFeatureBindingCollection<IRainfallGenerator> generators = catchmentModel.getGenerators();

    /* Loop the generators. */
    for( final IRainfallGenerator generator : generators )
    {
      if( existing )
        m_mappingsHelper.addExistingGenerator( scenario, generator );
      else
        m_mappingsHelper.addGenerator( scenario, generator );
    }
  }

  private void analyzeMappings( final IScenario scenario, final boolean existing ) throws Exception
  {
    /* The rrm scenario. */
    final RrmScenario rrmScenario = new RrmScenario( scenario.getFolder() );

    /* Load the timeseries mappings. */
    final IResource timeseriesMappingsGml = rrmScenario.getTimeseriesMappingsGml();
    final GMLWorkspace timeseriesMappingsWorkspace = GmlSerializer.createGMLWorkspace( (IFile) timeseriesMappingsGml );
    final ITimeseriesMappingCollection timeseriesMappingCollection = (ITimeseriesMappingCollection) timeseriesMappingsWorkspace.getRootFeature();
    final IFeatureBindingCollection<ITimeseriesMapping> timeseriesMappings = timeseriesMappingCollection.getTimeseriesMappings();

    /* Loop the timeseries mappings. */
    for( final ITimeseriesMapping timeseriesMapping : timeseriesMappings )
    {
      if( existing )
        m_mappingsHelper.addExistingMapping( scenario, timeseriesMapping );
      else
        m_mappingsHelper.addMapping( scenario, timeseriesMapping );
    }
  }

  public IStatus createMappings( IProgressMonitor monitor )
  {
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    /* The status collector. */
    final IStatusCollector collector = new StatusCollectorWithTime( KalypsoUIRRMPlugin.getID() );

    try
    {
      /* Monitor. */
      monitor.beginTask( Messages.getString( "MergeMappingsWorker_5" ), 500 ); //$NON-NLS-1$
      monitor.subTask( Messages.getString( "MergeMappingsWorker_6" ) ); //$NON-NLS-1$

      /* The target rrm scenario. */
      final RrmScenario targetRrmScenario = new RrmScenario( m_targetScenario.getFolder() );

      /* Load the target catchment models. */
      final IResource targetCatchmentModelsGml = targetRrmScenario.getCatchmentModelsGml();
      final GMLWorkspace targetCatchmentModelsWorkspace = GmlSerializer.createGMLWorkspace( (IFile) targetCatchmentModelsGml );
      final ICatchmentModel targetCatchmentModel = (ICatchmentModel) targetCatchmentModelsWorkspace.getRootFeature();
      final IFeatureBindingCollection<IRainfallGenerator> targetGenerators = targetCatchmentModel.getGenerators();

      /* Load the target timeseries mappings. */
      final IResource targetTimeseriesMappingsGml = targetRrmScenario.getTimeseriesMappingsGml();
      final GMLWorkspace targetTimeseriesMappingsWorkspace = GmlSerializer.createGMLWorkspace( (IFile) targetTimeseriesMappingsGml );
      final ITimeseriesMappingCollection targetTimeseriesMappingCollection = (ITimeseriesMappingCollection) targetTimeseriesMappingsWorkspace.getRootFeature();
      final IFeatureBindingCollection<ITimeseriesMapping> targetTimeseriesMappings = targetTimeseriesMappingCollection.getTimeseriesMappings();

      /* Monitor. */
      monitor.worked( 250 );
      monitor.subTask( Messages.getString( "MergeMappingsWorker_7" ) ); //$NON-NLS-1$

      /* Add the catchment models and the timeseries mappings. */
      addGenerators( targetGenerators );
      addMappings( targetTimeseriesMappings );

      /* Monitor. */
      monitor.worked( 250 );

      /* Save the target catchment models. */
      GmlSerializer.saveWorkspace( targetCatchmentModelsWorkspace, (IFile) targetCatchmentModelsGml );

      /* Save the target timeseries mappings. */
      GmlSerializer.saveWorkspace( targetTimeseriesMappingsWorkspace, (IFile) targetTimeseriesMappingsGml );

      return collector.asMultiStatus( Messages.getString( "MergeMappingsWorker_8" ) ); //$NON-NLS-1$
    }
    catch( final Exception ex )
    {
      collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex ) );
      return collector.asMultiStatus( Messages.getString( "MergeMappingsWorker_9" ) ); //$NON-NLS-1$
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  private void addGenerators( final IFeatureBindingCollection<IRainfallGenerator> targetGenerators ) throws Exception
  {
    /* Get the generator keys. */
    final GeneratorKey[] generatorKeys = m_mappingsHelper.getGeneratorKeys();

    /* Update the generators. */
    for( final GeneratorKey generatorKey : generatorKeys )
    {
      final GeneratorValue generatorValue = m_mappingsHelper.getGeneratorValue( generatorKey );
      if( !generatorValue.isCopy() )
        continue;

      final Feature parentFeature = targetGenerators.getParentFeature();
      final FeatureList featureList = targetGenerators.getFeatureList();
      final IRainfallGenerator generator = generatorValue.getGenerator();

      final IRainfallGenerator newGenerator = (IRainfallGenerator) FeatureHelper.cloneFeature( parentFeature, featureList.getPropertyType(), generator );
      m_mappingsHelper.updateGenerators( generator, newGenerator );
    }

    /* After all generators has been updated, fix the links in the multi generators. */
    updateMultiGenerators( generatorKeys );
  }

  private void updateMultiGenerators( final GeneratorKey[] generatorKeys )
  {
    for( final GeneratorKey generatorKey : generatorKeys )
    {
      final GeneratorValue generatorValue = m_mappingsHelper.getGeneratorValue( generatorKey );
      final IRainfallGenerator generator = generatorValue.getGenerator();
      if( !(generator instanceof IMultiGenerator) )
        continue;

      /* If after updating the feature ids are still equal, this is a generator, which was not newly created. */
      /* It should look like this after the update, if generators were copied and replaced. */
      /* key: old feature id */
      /* value: new feature id */
      if( generatorKey.getFeatureId().equals( generatorValue.getFeatureId() ) )
        continue;

      final IMultiGenerator multiGenerator = (IMultiGenerator) generator;
      final FeatureList subGenerators = (FeatureList) multiGenerator.getProperty( IMultiGenerator.MEMBER_SUB_GENERATOR );
      if( subGenerators == null )
        continue;

      /* This is the path of the scenario where the multi generator and its sub generators were in. */
      final String scenarioPath = generatorKey.getScenarioPath();

      for( final Object object : subGenerators )
      {
        /* Sub generators may be a linear sum generator or a multi generator. */
        final IXLinkedFeature subGenerator = (IXLinkedFeature) object;

        /* This is the old feature id of the sub generator. */
        /* With it and the scenario path, we can find the value with the new generator. */
        final String featureId = subGenerator.getFeatureId();

        final String generatorHref = m_mappingsHelper.getGeneratorHref( scenarioPath, featureId );
        if( generatorHref != null )
        {
          final IRelationType relation = subGenerator.getParentRelation();
          final IFeatureType featureType = subGenerator.getFeatureType();
          subGenerator.setLink( relation, generatorHref, featureType );
        }
      }
    }
  }

  private void addMappings( final IFeatureBindingCollection<ITimeseriesMapping> targetTimeseriesMappings ) throws Exception
  {
    final MappingKey[] mappingKeys = m_mappingsHelper.getMappingKeys();
    for( final MappingKey mappingKey : mappingKeys )
    {
      final MappingValue mappingValue = m_mappingsHelper.getMappingValue( mappingKey );
      if( !mappingValue.isCopy() )
        continue;

      final Feature parentFeature = targetTimeseriesMappings.getParentFeature();
      final FeatureList featureList = targetTimeseriesMappings.getFeatureList();
      final ITimeseriesMapping mapping = mappingValue.getMapping();

      final ITimeseriesMapping newMapping = (ITimeseriesMapping) FeatureHelper.cloneFeature( parentFeature, featureList.getPropertyType(), mapping );
      m_mappingsHelper.updateMappings( mapping, newMapping );
    }
  }

  /**
   * @param newSimulationFolders
   *          A map <scenarioUri_simulationName, newSimulationFolder>.
   */
  public IStatus createSimulations( final Map<String, IFolder> newSimulationFolders, IProgressMonitor monitor )
  {
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    /* The status collector. */
    final IStatusCollector collector = new StatusCollectorWithTime( KalypsoUIRRMPlugin.getID() );

    try
    {
      /* Monitor. */
      monitor.beginTask( Messages.getString( "MergeMappingsWorker_10" ), 250 * m_sourceScenarios.length ); //$NON-NLS-1$

      /* The target rrm scenario. */
      final RrmScenario targetRrmScenario = new RrmScenario( m_targetScenario.getFolder() );

      /* Load the simulations of the target. */
      final IFile targetSimulationsGml = targetRrmScenario.getSimulationsGml();
      final GMLWorkspace targetSimulationsWorkspace = GmlSerializer.createGMLWorkspace( targetSimulationsGml );
      final SimulationCollection targetSimulationCollection = (SimulationCollection) targetSimulationsWorkspace.getRootFeature();

      /* Loop the source scenarios. */
      for( final IScenario sourceScenario : m_sourceScenarios )
      {
        /* Monitor. */
        monitor.subTask( String.format( Messages.getString( "MergeMappingsWorker_11" ), sourceScenario.getName() ) ); //$NON-NLS-1$

        /* The source rrm scenario. */
        final RrmScenario sourceRrmScenario = new RrmScenario( sourceScenario.getFolder() );

        /* Load the simulations of the source. */
        final IFile sourceSimulationsGml = sourceRrmScenario.getSimulationsGml();
        final GMLWorkspace sourceSimulationsWorkspace = GmlSerializer.createGMLWorkspace( sourceSimulationsGml );
        final SimulationCollection sourceSimulationCollection = (SimulationCollection) sourceSimulationsWorkspace.getRootFeature();

        /* Get the simulations of the source. */
        final IFeatureBindingCollection<NAControl> sourceSimulations = sourceSimulationCollection.getSimulations();
        for( final NAControl sourceSimulation : sourceSimulations )
        {
          /* Add the meta control. */
          final NAControl targetSimulation = targetSimulationCollection.getSimulations().addNew( NAControl.FEATURE_NACONTROL );
          FeatureHelper.copyData( sourceSimulation, targetSimulation );

          /* Update the hrefs. */
          updateHrefs( sourceScenario, sourceSimulation, targetSimulation );

          /* Update the description. */
          final String key = String.format( "%s_%s", sourceScenario.getURI(), sourceSimulation.getDescription() ); //$NON-NLS-1$
          final IFolder simulationFolder = newSimulationFolders.get( key );
          if( simulationFolder != null )
            targetSimulation.setDescription( simulationFolder.getName() );
        }

        /* Monitor. */
        monitor.worked( 250 );
      }

      /* Save the target simulations. */
      GmlSerializer.saveWorkspace( targetSimulationsWorkspace, targetSimulationsGml );

      return collector.asMultiStatus( Messages.getString( "MergeMappingsWorker_13" ) ); //$NON-NLS-1$
    }
    catch( final Exception ex )
    {
      collector.add( new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex ) );
      return collector.asMultiStatus( Messages.getString( "MergeMappingsWorker_14" ) ); //$NON-NLS-1$
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  private void updateHrefs( final IScenario sourceScenario, final NAControl sourceSimulation, final NAControl targetSimulation )
  {
    final String targetHrefN = getGeneratorHref( sourceScenario, sourceSimulation, NAControl.PROP_GENERATOR_N );
    if( targetHrefN != null )
      targetSimulation.setGeneratorReferenceN( targetHrefN );

    final String targetHrefT = getGeneratorHref( sourceScenario, sourceSimulation, NAControl.PROP_GENERATOR_T );
    if( targetHrefT != null )
      targetSimulation.setGeneratorReferenceT( targetHrefT );

    final String targetHrefE = getGeneratorHref( sourceScenario, sourceSimulation, NAControl.PROP_GENERATOR_E );
    if( targetHrefE != null )
      targetSimulation.setGeneratorReferenceE( targetHrefE );

    final String targetHrefGauge = getMappingHref( sourceScenario, sourceSimulation, NAControl.PROPERTY_MAPPING_GAUGE );
    if( targetHrefGauge != null )
      targetSimulation.setMappingReferenceGauge( targetHrefGauge );

    final String targetHrefNodeInflow = getMappingHref( sourceScenario, sourceSimulation, NAControl.PROPERTY_MAPPING_NODE_INFLOW );
    if( targetHrefNodeInflow != null )
      targetSimulation.setMappingReferenceNodeInflow( targetHrefNodeInflow );

    final String targetHrefStorageEvaporation = getMappingHref( sourceScenario, sourceSimulation, NAControl.PROPERTY_MAPPING_STORAGE_EVAPORATION );
    if( targetHrefStorageEvaporation != null )
      targetSimulation.setMappingReferenceStorageEvaporation( targetHrefStorageEvaporation );
  }

  private String getGeneratorHref( final IScenario sourceScenario, final NAControl sourceSimulation, final QName linkProperty )
  {
    final IXLinkedFeature generatorLink = (IXLinkedFeature) sourceSimulation.getProperty( linkProperty );
    if( generatorLink == null )
      return null;

    return m_mappingsHelper.getGeneratorHref( sourceScenario, generatorLink.getFeatureId() );
  }

  private String getMappingHref( final IScenario sourceScenario, final NAControl sourceSimulation, final QName linkProperty )
  {
    final IXLinkedFeature mappingLink = (IXLinkedFeature) sourceSimulation.getProperty( linkProperty );
    if( mappingLink == null )
      return null;

    return m_mappingsHelper.getMappingHref( sourceScenario, mappingLink.getFeatureId() );
  }
}