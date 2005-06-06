package com.bce.eind.core.result.impl;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import com.bce.eind.core.result.IResult;

public class Result implements IResult
{
  private final Map<Double, Double> m_map = new HashMap<Double, Double>();

  private final Map<Double, Double> m_unmodMap = Collections.unmodifiableMap( m_map );

  private final TYPE m_type;

  private final String m_name;

  public Result( final String name, final IResult.TYPE type )
  {
    m_name = name;
    m_type = type;
  }

  public String getName( )
  {
    return m_name;
  }

  public Double getResult( final double station )
  {
    return m_map.get( station );
  }

  public void addResult( final double station, final double value )
  {
    m_map.put( station, value );
  }

  public Double removeResult( final double station )
  {
    return m_map.remove( station );
  }

  public TYPE getType( )
  {
    return m_type;
  }

  public Iterator<Double> iterator( )
  {
    return m_unmodMap.keySet().iterator();
  }
}
