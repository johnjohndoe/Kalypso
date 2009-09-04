package org.kalypso.model.wspm.core.result.impl;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.kalypso.model.wspm.core.result.IResultSet;
import org.kalypso.model.wspm.core.result.IStationResult;


public class Result implements IResultSet
{
  private final Map<String, Map<String, Double>> m_map = new HashMap<String, Map<String, Double>>();

  private final Map<String, Map<String, Double>> m_unmodMap = Collections.unmodifiableMap( m_map );

  private final String m_name;

  public Result( final String name )
  {
    m_name = name;
  }

  public String getName( )
  {
    return m_name;
  }

  public Double getValue( final String station, final String type )
  {
    final Map<String, Double> map = m_map.get( station );
    return map == null ? null : map.get( type );
  }

  public void addResult( final String station, final double value, final String type )
  {
    final Map<String, Double> map = getResults( station );

    map.put( type, value );
  }

  /** Return the results for the given station. If no such map exists, creates one. */
  private Map<String, Double> getResults( final String station )
  {
    final Map<String, Double> map = m_map.get( station );
    if( map != null )
      return map;

    final Map<String, Double> newMap = new HashMap<String, Double>(  );
    m_map.put( station, newMap );

    return newMap;
  }

  /** Returns an iterator over the stations. */
  public Iterator<String> iterator( )
  {
    return m_unmodMap.keySet().iterator();
  }

  public IStationResult getValues( final String station )
  {
    final Map<String, Double> results = getResults( station );
    return new StationResult( m_name, Collections.unmodifiableMap( results ) );
  }

  public Double putValue( final String station, final String type, final double value )
  {
    final Map<String, Double> results = getResults( station );
    return results.put( type, value );
  }
}
