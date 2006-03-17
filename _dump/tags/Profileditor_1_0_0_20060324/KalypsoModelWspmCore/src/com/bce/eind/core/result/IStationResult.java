package com.bce.eind.core.result;

import com.bce.eind.core.result.IResultSet.TYPE;

public interface IStationResult
{
  public String getName();
  
  public TYPE[] getTypes();

  public Double getValue( final IResultSet.TYPE type );
}