package org.kalypso.ogc.sensor.filter.filters;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;

/**
 * ValueFilter
 * 
 * @author schlienger
 */
public class ValueFilter extends AbstractObservationFilter
{
  /**
   * @see org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter#initFilter(java.lang.Object,
   *      org.kalypso.ogc.sensor.IObservation)
   */
  public void initFilter( Object conf, IObservation obs )
      throws SensorException
  {
    super.initFilter( conf, obs );
    
    
  }
}