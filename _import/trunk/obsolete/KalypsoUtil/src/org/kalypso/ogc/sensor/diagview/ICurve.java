package org.kalypso.ogc.sensor.diagview;


/**
 * Represents a curve in a diagram
 * 
 * @author schlienger
 *
 */
public interface ICurve
{
  public String getName();
  
  public IAxisMapping[] getMappings();
}
