package org.kalypso.ogc.sensor;

import java.util.List;

/**
 * Eine sog. Observation im Sinne von OGC Sensor-ML. Beschreibt eine maschinelle oder 
 * menschlische Wert-Erfassung.
 * 
 * @author schlienger
 */
public interface IObservation
{
  public String getName();
  
  public ITarget getTarget();
  
  public Metadata getMetadata();
  
  public List getAxisList();
}
