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
import org.kalypso.ui.rrm.internal.timeseries.binding.Station;
import org.kalypso.ui.rrm.internal.timeseries.binding.StationCollection;
import org.kalypso.ui.rrm.internal.timeseries.binding.Timeseries;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class StationsByStationsStrategy
{
  private final StationCollection m_stations;

  private final ITimeseriesTreeModel m_model;

  public StationsByStationsStrategy( final ITimeseriesTreeModel model, final StationCollection stations )
  {
    m_model = model;
    m_stations = stations;
  }

  public TimeseriesNode buildNodes( )
  {
    final TimeseriesNode virtualRootNode = new TimeseriesNode( m_model, null, null, null );

    final Map<String, Station[]> stationGroups = groupStations();
    if( stationGroups.size() == 1 && stationGroups.containsKey( null ) )
      buildStations( virtualRootNode, stationGroups.get( null ) );
    else
      buildGroupNodes( virtualRootNode, stationGroups );

    return virtualRootNode;
  }

  private void buildStations( final TimeseriesNode parent, final Station[] stations )
  {
    for( final Station station : stations )
    {
      final TimeseriesNode stationNode = buildStationNode( parent, station );
      parent.addChild( stationNode );
    }
  }

  private void buildGroupNodes( final TimeseriesNode parent, final Map<String, Station[]> stationGroups )
  {
    for( final Entry<String, Station[]> entry : stationGroups.entrySet() )
    {
      final String group = entry.getKey();
      final Station[] stations = entry.getValue();

      final ITimeseriesNodeUiHandler uiHandler = new GroupUiHandler( group );

      final TimeseriesNode groupNode = new TimeseriesNode( m_model, parent, uiHandler, entry );

      buildStations( groupNode, stations );

      parent.addChild( groupNode );
    }
  }

  private Map<String, Station[]> groupStations( )
  {
    final Map<String, Station[]> groups = new HashMap<>();

    final IFeatureBindingCollection<Station> stations = m_stations.getStations();
    for( final Station station : stations )
    {
      final String group = station.getGroup();
      if( !groups.containsKey( group ) )
        groups.put( group, new Station[0] );

      final Station[] oldStations = groups.get( group );
      final Station[] newStations = ArrayUtils.add( oldStations, station );
      groups.put( group, newStations );
    }

    return groups;
  }

  private TimeseriesNode buildStationNode( final TimeseriesNode parent, final Station station )
  {
    final ITimeseriesNodeUiHandler uiHandler = new StationUiHandler( m_model, station );

    final TimeseriesNode stationNode = new TimeseriesNode( m_model, parent, uiHandler, station );

    final Map<String, Timeseries[]> parameters = groupByParameter( station );

    for( final Entry<String, Timeseries[]> entry : parameters.entrySet() )
    {
      final TimeseriesNode parameterNode = buildParameterNode( station, stationNode, entry );
      stationNode.addChild( parameterNode );
    }

    return stationNode;
  }

  private Map<String, Timeseries[]> groupByParameter( final Station station )
  {
    final Map<String, Timeseries[]> parameters = new HashMap<>();

    final IFeatureBindingCollection<Timeseries> allTimeseries = station.getTimeseries();

    for( final Timeseries timeseries : allTimeseries )
    {
      final String parameterType = timeseries.getParameterType();

      if( !parameters.containsKey( parameterType ) )
        parameters.put( parameterType, new Timeseries[0] );

      final Timeseries[] oldTimeseriesArray = parameters.get( parameterType );

      final Timeseries[] newTimeseriesArray = ArrayUtils.add( oldTimeseriesArray, timeseries );
      parameters.put( parameterType, newTimeseriesArray );
    }

    return parameters;
  }

  private TimeseriesNode buildParameterNode( final Station station, final TimeseriesNode parent, final Entry<String, Timeseries[]> entry )
  {
    final String parameterType = entry.getKey();
    final Timeseries[] timeseries = entry.getValue();

    final ITimeseriesNodeUiHandler uiHandler = new ParameterUiHandler( m_model, station, parameterType, timeseries );
    final TimeseriesNode parameterNode = new TimeseriesNode( m_model, parent, uiHandler, parameterType );

    for( final Timeseries timeserie : timeseries )
    {
      final TimeseriesNode timeseriesNode = buildTimeseriesNode( parameterNode, timeserie );
      parameterNode.addChild( timeseriesNode );
    }

    return parameterNode;
  }

  private TimeseriesNode buildTimeseriesNode( final TimeseriesNode parent, final Timeseries timeseries )
  {
    final ITimeseriesNodeUiHandler uiHandler = new TimeseriesUiHandler( m_model, timeseries );

    return new TimeseriesNode( m_model, parent, uiHandler, timeseries );
  }
}