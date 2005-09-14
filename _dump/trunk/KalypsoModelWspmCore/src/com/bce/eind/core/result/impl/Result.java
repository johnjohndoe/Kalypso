package com.bce.eind.core.result.impl;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import com.bce.eind.core.result.IResultSet;
import com.bce.eind.core.result.IStationResult;

public class Result implements IResultSet
{
  private final Map<Double, Map<TYPE, Double>> m_map = new HashMap<Double, Map<TYPE, Double>>();

  private final Map<Double, Map<TYPE, Double>> m_unmodMap = Collections.unmodifiableMap( m_map );

  private final String m_name;

  public Result( final String name )
  {
    m_name = name;
  }

  public String getName( )
  {
    return m_name;
  }

  public Double getValue( final double station, final TYPE type )
  {
    final Map<TYPE, Double> map = m_map.get( station );
    return map == null ? null : map.get( type );
  }

  public void addResult( final double station, final double value, final TYPE type )
  {
    final Map<TYPE, Double> map = getResults( station );

    map.put( type, value );
  }

  /** Return the results for the given station. If no such map exists, creates one. */
  private Map<TYPE, Double> getResults( final double station )
  {
    final Map<TYPE, Double> map = m_map.get( station );
    if( map != null )
      return map;

    final Map<TYPE, Double> newMap = new HashMap<TYPE, Double>( TYPE.values().length );
    m_map.put( station, newMap );

    return newMap;
  }

  // public Double removeResult( final double station )
  // {
  // return m_map.remove( station );
  // }

  /** Returns an iterator over the stations. */
  public Iterator<Double> iterator( )
  {
    return m_unmodMap.keySet().iterator();
  }

  public IStationResult getValues( final double station )
  {
    final Map<TYPE, Double> results = getResults( station );
    return new StationResult( m_name, Collections.unmodifiableMap( results ) );
  }

  public Double putValue( final double station, final TYPE type, final double value )
  {
    final Map<TYPE, Double> results = getResults( station );
    return results.put( type, value );
  }
}
