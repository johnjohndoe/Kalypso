package org.kalypso.ogc.sensor.filter;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;

/**
 * IObservationFilter
 * <p>
 * The specification of the filter is coded as an xml string.
 * 
 * @author schlienger
 */
public interface IObservationFilter extends IObservation
{
  /**
   * Initializes the filter with the given configuration and the
   * observation.
   * 
   * @param conf implementation dependent configuration.
   * @param obs observation that will be filtered. Can also be a subclass of IObservationFiler.
   * @throws SensorException
   */
  public void initFilter( final Object conf, final IObservation obs ) throws SensorException;
}