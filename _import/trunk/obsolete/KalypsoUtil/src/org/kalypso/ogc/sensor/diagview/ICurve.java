package org.kalypso.ogc.sensor.diagview;

import org.kalypso.ogc.sensor.IObservationProvider;


/**
 * Represents a curve in a diagram
 * 
 * @author schlienger
 */
public interface ICurve extends IObservationProvider
{
  public String getName();
  
  public IAxisMapping[] getMappings();
}
