package org.kalypso.model.wspm.core.result;

import org.kalypso.model.wspm.core.result.IResultSet.TYPE;

public interface IStationResult
{
  public String getName();
  
  public TYPE[] getTypes();

  public Double getValue( final IResultSet.TYPE type );
}