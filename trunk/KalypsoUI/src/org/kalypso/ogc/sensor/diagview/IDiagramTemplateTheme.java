package org.kalypso.ogc.sensor.diagview;

import java.util.List;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * Observation theme. Contains a list of curves.
 * 
 * @author schlienger
 */
public interface IDiagramTemplateTheme
{
  /**
   * @return observation on which this theme is based
   */
  public IObservation getObservation();
  
  /**
   * @return [optional] variable arguments that can be used when values are fetched from
   * the observation
   */
  public IVariableArguments getArguments( );
  
  /**
   * @return list of <code>IDiagramCurve</code>
   */
  public List getCurves();

  /**
   * disposes this theme
   */
  public void dispose( );
}
