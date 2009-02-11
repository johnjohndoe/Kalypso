package org.kalypso.model.wspm.core.result.impl;

import java.util.Map;

import org.kalypso.model.wspm.core.result.IStationResult;

public class StationResult implements IStationResult
{
  private final String m_name;

  private final Map<String, Double> m_results;

  public StationResult( final String name, final Map<String, Double> results )
  {
    m_name = name;
    m_results = results;
  }

  public String getName( )
  {
    return m_name;
  }

  /**
   * @see org.kalypso.model.wspm.core.result.IStationResult#getComponentIds()
   */
  public String[] getComponentIds( )
  {
    return m_results.keySet().toArray( new String[m_results.size()] );
  }

  /**
   * @see org.kalypso.model.wspm.core.result.IStationResult#getComponentName(java.lang.String)
   */
  public String getComponentName( final String componentId )
  {
    return componentId;
  }

  /**
   * @see org.kalypso.model.wspm.core.result.IStationResult#getComponentValue(java.lang.String)
   */
  public Double getComponentValue( final String componentId )
  {
    return m_results.get( componentId );
  }

}
