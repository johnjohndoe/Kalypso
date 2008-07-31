package org.kalypso.model.wspm.sobek.result.processing.interfaces;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.sobek.result.processing.model.IResultTimeSeries;

public interface IPolderNodeResultWrapper
{
  public enum POLDER_NODE_RESULT
  {
    eInflow,
    eOutflow;

    public String getPostfix( )
    {
      POLDER_NODE_RESULT type = valueOf( name() );
      if( POLDER_NODE_RESULT.eInflow.equals( type ) )
        return "_inflow";
      else if( POLDER_NODE_RESULT.eOutflow.equals( type ) )
        return "_outflow";

      throw new IllegalStateException();
    }
  }

  void init( ) throws CoreException;

  IResultTimeSeries getInflow( ) throws CoreException;

  IResultTimeSeries getOutflow( ) throws CoreException;
}
