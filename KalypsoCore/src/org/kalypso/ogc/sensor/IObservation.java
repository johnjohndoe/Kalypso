package org.kalypso.ogc.sensor;

import org.kalypso.util.runtime.IVariableArguments;
import org.kalypso.util.xml.xlink.IXlink;

/**
 * Eine sog. Observation im Sinne von OGC Sensor-ML. Beschreibt eine maschinelle
 * oder menschlische Wert-Erfassung.
 * 
 * @author schlienger
 */
public interface IObservation
{
  /**
   * Returns the identifier of this Observation. The identifier can be used to
   * uniquely identify the Observation within its repository.
   * 
   * @return identifier
   */
  public String getIdentifier();

  /**
   * Returns the name of this Observation
   * 
   * @return name
   */
  public String getName();

  /**
   * Returns true if this observation is editable.
   * 
   * @return editable flag
   */
  public boolean isEditable();

  /**
   * Returns the target object for which this observation has measurements.
   * 
   * @return target or null
   */
  public IXlink getTarget();

  /**
   * Returns the list of Metadata.
   * 
   * @return metadata
   */
  public MetadataList getMetadataList();

  /**
   * Returns the list of axis
   * 
   * @return axes array
   */
  public IAxis[] getAxisList();

  /**
   * Returns the values resulting from the measurements this observation stands
   * for.
   * 
   * @param args
   *          some client defined arguments that can be interpretated by the
   *          implementation. Implementors of this interface can use this
   *          parameter, but they are not forced to do so.
   * @return model
   * @throws SensorException
   */
  public ITuppleModel getValues( IVariableArguments args ) throws SensorException;

  /**
   * Sets the given values.
   * 
   * @param values
   * @throws SensorException
   */
  public void setValues( ITuppleModel values ) throws SensorException;
}