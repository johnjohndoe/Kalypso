package org.kalypso.ogc.sensor;

import org.kalypso.util.runtime.IVariableArguments;

/**
 * Eine sog. Observation im Sinne von OGC Sensor-ML. Beschreibt eine maschinelle
 * oder menschlische Wert-Erfassung.
 * 
 * @author schlienger
 */
public interface IObservation
{
  /**
   * Returns the name of this Observation
   */
  public String getName();

  /**
   * Returns true if this observation is editable.
   */
  public boolean isEditable();

  /**
   * Returns the target object for which this observation has measurements.
   */
  public ITarget getTarget();

  /**
   * Returns the list of Metadata.
   */
  public Metadata getMetadata();

  /**
   * Returns the list of axis
   */
  public IAxis[] getAxisList();

  /**
   * Returns the values resulting from the measurements this observation stands
   * for.
   * 
   * @param args
   *          some client defined arguments that can be interpretated by the
   *          implementation.
   */
  public ITuppleModel getValues( IVariableArguments args ) throws SensorException;

  /**
   * Sets the given values.
   */
  public void setValues( ITuppleModel values ) throws SensorException;
}