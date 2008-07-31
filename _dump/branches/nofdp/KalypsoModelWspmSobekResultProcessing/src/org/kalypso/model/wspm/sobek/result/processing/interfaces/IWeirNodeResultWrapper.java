package org.kalypso.model.wspm.sobek.result.processing.interfaces;

import org.eclipse.core.runtime.CoreException;

public interface IWeirNodeResultWrapper
{

  public enum WEIR_NODE_RESULT
  {
    eDischarge,
    eWaterLevelAbove,
    eWaterLevelBelow;
  }

  void init( ) throws CoreException;

}
