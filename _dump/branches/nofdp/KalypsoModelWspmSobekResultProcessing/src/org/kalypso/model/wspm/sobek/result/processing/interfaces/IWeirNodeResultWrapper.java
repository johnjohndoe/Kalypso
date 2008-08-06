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
        return "_discharge"; //$NON-NLS-1$
      else if( WEIR_NODE_RESULT.eWaterLevelAbove.equals( type ) )
        return "_wl_above"; //$NON-NLS-1$
      else if( WEIR_NODE_RESULT.eWaterLevelBelow.equals( type ) )
        return "_wl_below"; //$NON-NLS-1$

      throw new IllegalStateException();
    }
  }

  void init( ) throws CoreException;

  public IResultTimeSeries getDischarge( ) throws CoreException; // m³/sec

  public IResultTimeSeries getWaterLevelAbove( ) throws CoreException; // m NHN

  public IResultTimeSeries getWaterLevelBelow( ) throws CoreException; // m NHN
}
