package org.kalypso.ogc.sensor;

/**
 * Provides clients with the IObservation.
 * 
 * @author schlienger
 */
public interface IObservationProvider
{
  /**
   * Returns the IObservation which we want to manipulate
   */
  public IObservation getObservation();
}
