package org.kalypso.ogc.sensor;

/**
 * Provides clients with the IObservation and the default axis to display.
 * 
 * @author schlienger
 */
public interface IObservationProvider
{
  /**
   * Returns the IObservation which we want to manipulate
   */
  public IObservation getObservation();
  
  /**
   * Returns the default axis that is to be used when getting the values
   * of the associated IObservation
   */
  public IAxis getDisplayAxis();
}
