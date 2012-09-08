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

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

import org.apache.commons.lang3.ObjectUtils;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.binding.cm.IMultiGenerator;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMapping;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ui.rrm.internal.calccase.CatchmentModelHelper;

import de.renew.workflow.connector.cases.IScenario;

/**
 * Helper for merging catchment models and timeseries mappings.
 *
 * @author Holger Albert
 */
public class MergeMappingsHelper
{
  /**
   * The catchment models.
   */
  private final Map<GeneratorKey, GeneratorValue> m_generators;

  /**
   * The timeseries mappings.
   */
  private final Map<MappingKey, MappingValue> m_mappings;

  /**
   * The constructor.
   */
  public MergeMappingsHelper( )
  {
    m_generators = new LinkedHashMap<>();
    m_mappings = new LinkedHashMap<>();
  }

  public void addExistingGenerator( final IScenario scenario, final IRainfallGenerator generator )
  {
    /* Values for the key. */
    final String scenarioPath = scenario.getFolder().getFullPath().toOSString();
    final String featureId = generator.getId();

    /* Add the generator. Since it exists, there is no need to copy. */
    m_generators.put( new GeneratorKey( scenarioPath, featureId ), new GeneratorValue( generator, false ) );
  }

  public void addGenerator( final IScenario scenario, final IRainfallGenerator generator )
  {
    /* Values for the key. */
    final String scenarioPath = scenario.getFolder().getFullPath().toOSString();
    final String featureId = generator.getId();

    /* If there is a equal generator hashed, we will use this one. */
    final GeneratorValue existingGenerator = isEqualGeneratorAvailable( generator );
    if( existingGenerator != null )
    {
      /* This generator is equal to a existing one, or to one which should copied already, so we do not need to copy it. */
      m_generators.put( new GeneratorKey( scenarioPath, featureId ), new GeneratorValue( existingGenerator.getGenerator(), false ) );
      return;
    }

    /* No equal generator available. */
    /* This is a new one, so it must be copied. */
    m_generators.put( new GeneratorKey( scenarioPath, featureId ), new GeneratorValue( generator, true ) );
  }

  public GeneratorKey[] getGeneratorKeys( )
  {
    return m_generators.keySet().toArray( new GeneratorKey[] {} );
  }

  public GeneratorValue getGeneratorValue( final GeneratorKey generatorKey )
  {
    return m_generators.get( generatorKey );
  }

  public void updateGenerators( final IRainfallGenerator oldGenerator, final IRainfallGenerator newGenerator )
  {
    final GeneratorValue[] generatorValues = m_generators.values().toArray( new GeneratorValue[] {} );
    for( final GeneratorValue generatorValue : generatorValues )
    {
      /* Every generator value with the old generator contained, will be updated with the new generator. */
      final IRainfallGenerator generator = generatorValue.getGenerator();
      if( oldGenerator == generator )
        generatorValue.update( newGenerator );
    }
  }

  public String getGeneratorHref( final IScenario scenario, final String generatorId )
  {
    final String scenarioPath = scenario.getFolder().getFullPath().toOSString();
    return getGeneratorHref( scenarioPath, generatorId );
  }

  public String getGeneratorHref( final String scenarioPath, final String generatorId )
  {
    /* Create the generator key. */
    final GeneratorKey generatorKey = new GeneratorKey( scenarioPath, generatorId );

    /* Get the generator value. */
    final GeneratorValue generatorValue = m_generators.get( generatorKey );
    if( generatorValue == null )
      return null;

    return RrmScenario.FILE_CATCHMENT_MODELS_GML + "#" + generatorValue.getFeatureId(); //$NON-NLS-1$
  }

  private GeneratorValue isEqualGeneratorAvailable( final IRainfallGenerator generator )
  {
    final Collection<GeneratorValue> values = m_generators.values();
    for( final GeneratorValue value : values )
    {
      final IRainfallGenerator existingGenerator = value.getGenerator();

      final String parameterType = generator.getParameterType();
      final String existingParameterType = existingGenerator.getParameterType();
      if( !ObjectUtils.equals( parameterType, existingParameterType ) )
        continue;

      if( generator instanceof ILinearSumGenerator && existingGenerator instanceof ILinearSumGenerator )
      {
        if( CatchmentModelHelper.compareGeneratorCatchments( (ILinearSumGenerator) existingGenerator, (ILinearSumGenerator) generator, true ) )
          return value;
      }

      if( generator instanceof IMultiGenerator && existingGenerator instanceof IMultiGenerator )
      {
        if( CatchmentModelHelper.compareMultiGenerators( (IMultiGenerator) existingGenerator, (IMultiGenerator) generator ) )
          return value;
      }
    }

    return null;
  }

  public void addExistingMapping( final IScenario scenario, final ITimeseriesMapping mapping )
  {
    /* Values for the key. */
    final String scenarioPath = scenario.getFolder().getFullPath().toOSString();
    final String featureId = mapping.getId();

    /* Add the mapping. Since it exists, there is no need to copy. */
    m_mappings.put( new MappingKey( scenarioPath, featureId ), new MappingValue( mapping, false ) );
  }

  public void addMapping( final IScenario scenario, final ITimeseriesMapping mapping )
  {
    /* Values for the key. */
    final String scenarioPath = scenario.getFolder().getFullPath().toOSString();
    final String featureId = mapping.getId();

    /* If there is a equal mapping hashed, we will use this one. */
    final MappingValue existingMapping = isEqualMappingAvailable( mapping );
    if( existingMapping != null )
    {
      /* This mapping is equal to a existing one, or to one which should copied already, so we do not need to copy it. */
      m_mappings.put( new MappingKey( scenarioPath, featureId ), new MappingValue( existingMapping.getMapping(), false ) );
      return;
    }

    /* No equal mapping available. */
    /* This is a new one, so it must be copied. */
    m_mappings.put( new MappingKey( scenarioPath, featureId ), new MappingValue( mapping, true ) );
  }

  public MappingKey[] getMappingKeys( )
  {
    return m_mappings.keySet().toArray( new MappingKey[] {} );
  }

  public MappingValue getMappingValue( final MappingKey mappingKey )
  {
    return m_mappings.get( mappingKey );
  }

  public void updateMappings( final ITimeseriesMapping oldMapping, final ITimeseriesMapping newMapping )
  {
    final MappingValue[] mappingValues = m_mappings.values().toArray( new MappingValue[] {} );
    for( final MappingValue mappingValue : mappingValues )
    {
      /* Every mapping value with the old mapping contained, will be updated with the new mapping. */
      final ITimeseriesMapping mapping = mappingValue.getMapping();
      if( oldMapping == mapping )
        mappingValue.update( newMapping );
    }
  }

  public String getMappingHref( final IScenario scenario, final String mappingId )
  {
    /* Values for the key. */
    final String scenarioPath = scenario.getFolder().getFullPath().toOSString();
    final String featureId = mappingId;

    /* Create the mapping key. */
    final MappingKey mappingKey = new MappingKey( scenarioPath, featureId );

    /* Get the mapping value. */
    final MappingValue mappingValue = m_mappings.get( mappingKey );
    if( mappingValue == null )
      return null;

    return RrmScenario.FILE_TIMESERIES_MAPPINGS_GML + "#" + mappingValue.getFeatureId(); //$NON-NLS-1$
  }

  private MappingValue isEqualMappingAvailable( final ITimeseriesMapping mapping )
  {
    final Collection<MappingValue> values = m_mappings.values();
    for( final MappingValue value : values )
    {
      final ITimeseriesMapping existingMapping = value.getMapping();

      final String parameterType = mapping.getType().getLinkParameterType();
      final String existingParameterType = existingMapping.getType().getLinkParameterType();
      if( !ObjectUtils.equals( parameterType, existingParameterType ) )
        continue;

      if( CatchmentModelHelper.compareTimeseriesMappings( existingMapping, mapping ) )
        return value;
    }

    return null;
  }
}