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
package org.kalypso.ui.rrm.internal.cm.view;

import java.util.Collection;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.kalypso.model.hydrology.binding.cm.ICatchmentModel;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMapping;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMappingCollection;
import org.kalypso.model.hydrology.binding.timeseriesMappings.TimeseriesMappingType;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeStrategy;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeModel;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class TimeseriesMappingsTreeStrategy implements ITreeNodeStrategy
{
  private final ICatchmentModel m_catchmentModel;

  private final ITimeseriesMappingCollection m_timeseriesMappings;

  public TimeseriesMappingsTreeStrategy( final ICatchmentModel catchmentModel, final ITimeseriesMappingCollection timeseriesMappings )
  {
    m_catchmentModel = catchmentModel;
    m_timeseriesMappings = timeseriesMappings;
  }

  @Override
  public TreeNode buildNodes( final TreeNodeModel model )
  {
    final Map<Object, Collection<Object>> byParameterType = hashByType();

    final TreeNode rootNode = new TreeNode( model, null, null, this );

    if( byParameterType.size() == 0 )
      buildEmptyNode( rootNode );
    else
      buildNodes( rootNode, byParameterType );

    return rootNode;
  }

  private Map<Object, Collection<Object>> hashByType( )
  {
    final Comparator<Object> byTypeComparator = new TimeseriesMappingElementsComparator();

    final Map<Object, Collection<Object>> byType = new TreeMap<>( byTypeComparator );

    /* Fill known types: tree should never be empty */
    byType.put( ITimeseriesConstants.TYPE_MEAN_TEMPERATURE, new LinkedList<>() );
    byType.put( ITimeseriesConstants.TYPE_EVAPORATION_LAND_BASED, new LinkedList<>() );
    byType.put( ITimeseriesConstants.TYPE_RAINFALL, new LinkedList<>() );

    byType.put( TimeseriesMappingType.gaugeMeasurement, new LinkedList<>() );
    byType.put( TimeseriesMappingType.nodeInflow, new LinkedList<>() );
    byType.put( TimeseriesMappingType.storageEvaporation, new LinkedList<>() );

    /* Rainfall generators */
    final IFeatureBindingCollection<IRainfallGenerator> generators = m_catchmentModel.getGenerators();
    for( final IRainfallGenerator generator : generators )
    {
      final String parameterType = generator.getParameterType();
      if( parameterType != null )
      {
        if( byType.containsKey( parameterType ) )
        {
          final Collection<Object> generatorsOfType = byType.get( parameterType );
          generatorsOfType.add( generator );
        }
      }
    }

    /* Timeseries mappings */
    final IFeatureBindingCollection<ITimeseriesMapping> mappings = m_timeseriesMappings.getTimeseriesMappings();
    for( final ITimeseriesMapping mapping : mappings )
    {
      final TimeseriesMappingType type = mapping.getType();
      if( type != null )
      {
        if( byType.containsKey( type ) )
        {
          final Collection<Object> elements = byType.get( type );
          elements.add( mapping );
        }
      }
    }

    return byType;
  }

  private void buildEmptyNode( final TreeNode parent )
  {
    final TreeNode emptyNode = new TreeNode( parent, new EmptyNodeUiHandler( parent.getModel() ), new Object() );
    parent.addChild( emptyNode );
  }

  private void buildNodes( final TreeNode parent, final Map<Object, Collection<Object>> byParameterType )
  {
    for( final Entry<Object, Collection<Object>> entry : byParameterType.entrySet() )
    {
      final Object type = entry.getKey();
      final Collection<Object> elements = entry.getValue();

      final TreeNode node = buildNode( parent, type, elements, type );
      parent.addChild( node );
    }
  }

  private TreeNode buildNode( final TreeNode parent, final Object type, final Collection<Object> elements, final Object nodeData )
  {
    if( type instanceof String )
    {
      final String parameterType = (String) type;
      final IRainfallGenerator[] allGenerators = elements.toArray( new IRainfallGenerator[elements.size()] );

      final ParameterGeneratorUiHandler uiHandler = new ParameterGeneratorUiHandler( parent.getModel(), parameterType, allGenerators );
      final TreeNode parameterNode = new TreeNode( parent, uiHandler, nodeData );
      buildGeneratorNodes( parameterNode, allGenerators );

      return parameterNode;
    }

    if( type instanceof TimeseriesMappingType )
    {
      final TimeseriesMappingType mappingType = (TimeseriesMappingType) type;

      final ITimeseriesMapping[] allMappings = elements.toArray( new ITimeseriesMapping[elements.size()] );

      final TimeseriesMappingTypeUiHandler uiHandler = new TimeseriesMappingTypeUiHandler( parent.getModel(), mappingType );
      final TreeNode mappingNode = new TreeNode( parent, uiHandler, nodeData );
      buildMappingNodes( mappingNode, allMappings );
      return mappingNode;
    }

    throw new IllegalArgumentException();
  }

  private void buildGeneratorNodes( final TreeNode parent, final IRainfallGenerator[] allGenerators )
  {
    for( final IRainfallGenerator generator : allGenerators )
    {
      final GeneratorUiHandler uiHandler = new GeneratorUiHandler( parent.getModel(), generator );
      final TreeNode generatorNode = new TreeNode( parent, uiHandler, generator );
      parent.addChild( generatorNode );
    }
  }

  private void buildMappingNodes( final TreeNode parent, final ITimeseriesMapping[] allMappings )
  {
    for( final ITimeseriesMapping mapping : allMappings )
    {
      final TimeseriesMappingUiHandler uiHandler = new TimeseriesMappingUiHandler( parent.getModel(), mapping );
      final TreeNode generatorNode = new TreeNode( parent, uiHandler, mapping );
      parent.addChild( generatorNode );
    }
  }
}