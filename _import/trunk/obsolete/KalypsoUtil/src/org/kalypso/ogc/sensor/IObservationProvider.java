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
   * Shared axes are used when displaying many IObservation together. The Value axes are
   * merged on the shared axes basis.
   * 
   * @return the shared axis
   */
  public IAxis getSharedAxis();
  
  /**
   * Value axis are used when displaying many IObservations. Each IObservationProvider delivers
   * one and only one value axis that will be displayed "over" the shared axis.
   * 
   * @return the value axis
   */
  public IAxis getValueAxis();
}
