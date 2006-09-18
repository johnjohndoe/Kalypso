package org.kalypso.model.wspm.core.result;

public interface IStationResult
{
  public String[] getComponentIds();

  public String getName();
  
  public String getComponentName( final String componentId );

  public Number getComponentValue( final String componentId );
}