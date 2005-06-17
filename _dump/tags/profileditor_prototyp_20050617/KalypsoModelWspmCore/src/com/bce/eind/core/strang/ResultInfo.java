package com.bce.eind.core.strang;

import com.bce.eind.core.result.IResult;
import com.bce.eind.core.result.IResult.TYPE;

public class ResultInfo
{
  private final double m_value;

  private final String m_name;

  private final TYPE m_type;

  public ResultInfo( final IResult.TYPE type, final String name, final double value )
  {
    m_type = type;
    m_name = name;
    m_value = value;
  }

  public String getName( )
  {
    return m_name;
  }

  public TYPE getType( )
  {
    return m_type;
  }

  public double getValue( )
  {
    return m_value;
  }
}
