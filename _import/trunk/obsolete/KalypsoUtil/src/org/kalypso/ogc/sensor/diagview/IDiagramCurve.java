package org.kalypso.ogc.sensor.diagview;

import org.kalypso.ogc.sensor.IObservationProvider;
import org.kalypso.util.runtime.IVariableArguments;


/**
 * Represents a curve in a diagram
 * 
 * @author schlienger
 */
public interface IDiagramCurve extends IObservationProvider
{
  public String getName();
  
  public IAxisMapping[] getMappings();
  
  public IVariableArguments getArguments();
  public void setArguments( IVariableArguments args );
}
