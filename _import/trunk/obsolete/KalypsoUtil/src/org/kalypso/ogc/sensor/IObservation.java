package org.kalypso.ogc.sensor;

import org.kalypso.util.runtime.IVariableArguments;

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
  
  public IAxis[] getAxisList();
  
  public ITuppleModel getValues( IVariableArguments args ) throws SensorException;
  
  public void setValues( ITuppleModel values ) throws SensorException;
}
