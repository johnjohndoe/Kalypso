package org.kalypso.ogc.sensor;

import java.util.Date;
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
  
  // TODO: arguments from and to are not so nice here? better Visitor pattern?
  public ITuppleModel getValues( Date from, Date to ) throws SensorException;
  
  public void setValues( ITuppleModel values ) throws SensorException;
}
