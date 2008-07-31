package org.kalypso.model.wspm.sobek.result.processing.interfaces;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.sobek.result.processing.model.IResultTimeSeries;

public interface IRetardingBasinNodeResultWrapper
{
  public enum RETARDING_BASIN_NODE_RESULT
  {
    eWaterLevel,
    eHochwasserEntlastung,
    eGrundAblass;

    public String getPostfix( )
    {
      RETARDING_BASIN_NODE_RESULT type = valueOf( name() );
      if( RETARDING_BASIN_NODE_RESULT.eWaterLevel.equals( type ) )
        return "_waterlevel";
      else if( RETARDING_BASIN_NODE_RESULT.eHochwasserEntlastung.equals( type ) )
        return "_entlastung";
      else if( RETARDING_BASIN_NODE_RESULT.eGrundAblass.equals( type ) )
        return "_ablass";

      throw new IllegalStateException();
    }
  }

  public void init( ) throws CoreException;

  public IResultTimeSeries getWaterLevel( ) throws CoreException;

  public IResultTimeSeries getHochwasserEntlastung( ) throws CoreException;

  public IResultTimeSeries getGrundAblass( ) throws CoreException;

  public boolean isControlled( );
}
