package org.kalypso.model.wspm.sobek.result.processing.interfaces;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.sobek.result.processing.model.IResultTimeSeries;

public interface IWeirNodeResultWrapper
{

  public enum WEIR_NODE_RESULT
  {
    eDischarge,
    eWaterLevelAbove,
    eWaterLevelBelow;

    public String getPostfix( )
    {
      WEIR_NODE_RESULT type = valueOf( name() );
      if( WEIR_NODE_RESULT.eDischarge.equals( type ) )
        return "_discharge";
      else if( WEIR_NODE_RESULT.eWaterLevelAbove.equals( type ) )
        return "_wl_above";
      else if( WEIR_NODE_RESULT.eWaterLevelBelow.equals( type ) )
        return "_wl_below";

      throw new IllegalStateException();
    }
  }

  void init( ) throws CoreException;

  public IResultTimeSeries getDischarge( ) throws CoreException;

  public IResultTimeSeries getWaterLevelAbove( ) throws CoreException;

  public IResultTimeSeries getWaterLevelBelow( ) throws CoreException;
}
