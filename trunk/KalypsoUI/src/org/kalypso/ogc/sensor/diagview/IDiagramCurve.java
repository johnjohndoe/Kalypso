package org.kalypso.ogc.sensor.diagview;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.util.runtime.IVariableArguments;



/**
 * Represents a curve in a diagram
 * 
 * @author schlienger
 */
public interface IDiagramCurve
{
  /**
   * @return name of the curve as displayed in the diagram
   */
  public String getName();
  
  /**
   * @return list of mappings between diagram axes and observation axes
   */
  public IAxisMapping[] getMappings();
  
  /**
   * @return observation on which this curve is based
   */
  public IObservation getObservation();

  /**
   * @return [optional] variable arguments that can be used when values are fetched from
   * the observation
   */
  public IVariableArguments getArguments( );
}
