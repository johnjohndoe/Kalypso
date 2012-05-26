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
package org.kalypso.ui.rrm.internal.timeseries.view;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.lang3.ArrayUtils;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.IStationCollection;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeStrategy;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeUiHandler;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNodeModel;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class StationsByStationsStrategy implements ITreeNodeStrategy
{
  private final IStationCollection m_stations;

  public StationsByStationsStrategy( final IStationCollection stations )
  {
    m_stations = stations;
  }

  @Override
  public TreeNode buildNodes( final TreeNodeModel model )
  {
    final TreeNode virtualRootNode = new TreeNode( model, null, null, null );

    final Map<String, IStation[]> stationGroups = groupStations();

    if( stationGroups.size() == 0 )
      buildEmptyNode( model, virtualRootNode );
    else if( stationGroups.size() == 1 && stationGroups.containsKey( null ) )
      buildStations( model, virtualRootNode, stationGroups.get( null ) );
    else
      buildGroupNodes( model, virtualRootNode, stationGroups );

    return virtualRootNode;
  }

  private void buildEmptyNode( final TreeNodeModel model, final TreeNode parent )
  {
    final TreeNode emptyNode = new TreeNode( model, parent, new EmptyNodeUiHandler( model ), new Object() );
    parent.addChild( emptyNode );
  }

  private void buildStations( final TreeNodeModel model, final TreeNode parent, final IStation[] stations )
  {
    for( final IStation station : stations )
    {
      final TreeNode stationNode = buildStationNode( model, parent, station );
      parent.addChild( stationNode );
    }
  }

  private void buildGroupNodes( final TreeNodeModel model, final TreeNode parent, final Map<String, IStation[]> stationGroups )
  {
    for( final Entry<String, IStation[]> entry : stationGroups.entrySet() )
    {
      final String group = entry.getKey();
      final IStation[] stations = entry.getValue();

      final ITreeNodeUiHandler uiHandler = new GroupUiHandler( model, group );
      final TreeNode groupNode = new TreeNode( model, parent, uiHandler, entry );

      buildStations( model, groupNode, stations );

      parent.addChild( groupNode );
    }
  }

  private Map<String, IStation[]> groupStations( )
  {
    final Map<String, IStation[]> groups = new HashMap<>();

    final IFeatureBindingCollection<IStation> stations = m_stations.getStations();
    for( final IStation station : stations )
    {
      final String group = station.getGroup();
      if( !groups.containsKey( group ) )
        groups.put( group, new IStation[0] );

      final IStation[] oldStations = groups.get( group );
      final IStation[] newStations = ArrayUtils.add( oldStations, station );
      groups.put( group, newStations );
    }

    return groups;
  }

  private TreeNode buildStationNode( final TreeNodeModel model, final TreeNode parent, final IStation station )
  {
    final ITreeNodeUiHandler uiHandler = new StationUiHandler( model, station );

    final TreeNode stationNode = new TreeNode( model, parent, uiHandler, station );

    final Map<String, ITimeseries[]> parameters = groupByParameter( station );

    for( final Entry<String, ITimeseries[]> entry : parameters.entrySet() )
    {
      final TreeNode parameterNode = buildParameterNode( model, station, stationNode, entry );
      stationNode.addChild( parameterNode );
    }

    return stationNode;
  }

  private Map<String, ITimeseries[]> groupByParameter( final IStation station )
  {
    final Map<String, ITimeseries[]> parameters = new HashMap<>();

    final IFeatureBindingCollection<ITimeseries> allTimeseries = station.getTimeseries();

    for( final ITimeseries timeseries : allTimeseries )
    {
      final String parameterType = timeseries.getParameterType();

      if( !parameters.containsKey( parameterType ) )
        parameters.put( parameterType, new ITimeseries[0] );

      final ITimeseries[] oldTimeseriesArray = parameters.get( parameterType );

      final ITimeseries[] newTimeseriesArray = ArrayUtils.add( oldTimeseriesArray, timeseries );
      parameters.put( parameterType, newTimeseriesArray );
    }

    return parameters;
  }

  private TreeNode buildParameterNode( final TreeNodeModel model, final IStation station, final TreeNode parent, final Entry<String, ITimeseries[]> entry )
  {
    final String parameterType = entry.getKey();
    final ITimeseries[] timeseries = entry.getValue();

    final ITreeNodeUiHandler uiHandler = new ParameterUiHandler( model, station, parameterType, timeseries );
    final TreeNode parameterNode = new TreeNode( model, parent, uiHandler, parameterType );

    for( final ITimeseries timeserie : timeseries )
    {
      final TreeNode timeseriesNode = buildTimeseriesNode( model, parameterNode, timeserie );
      parameterNode.addChild( timeseriesNode );
    }

    return parameterNode;
  }

  private TreeNode buildTimeseriesNode( final TreeNodeModel model, final TreeNode parent, final ITimeseries timeseries )
  {
    final ITreeNodeUiHandler uiHandler = new TimeseriesUiHandler( model, timeseries );

    return new TreeNode( model, parent, uiHandler, timeseries );
  }
}