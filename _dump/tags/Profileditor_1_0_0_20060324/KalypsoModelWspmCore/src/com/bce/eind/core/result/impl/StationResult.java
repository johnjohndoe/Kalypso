package com.bce.eind.core.result.impl;

import java.util.Map;

import com.bce.eind.core.result.IStationResult;
import com.bce.eind.core.result.IResultSet.TYPE;

public class StationResult implements IStationResult
{
  private final String m_name;

  private final Map<TYPE, Double> m_results;

  public StationResult( final String name, final Map<TYPE, Double> results )
  {
    m_name = name;
    m_results = results;
  }

  public String getName( )
  {
    return m_name;
  }

  public TYPE[] getTypes( )
  {
    return m_results.keySet().toArray( new TYPE[m_results.size()] );
  }

  public Double getValue( final TYPE type )
  {
    return m_results.get( type );
  }

}
