package org.kalypso.ogc.sensor.filter;

import org.kalypso.ogc.sensor.IObservation;

/**
 * IObservationFilter
 * 
 * @author schlienger
 */
public interface IObservationFilter extends IObservation
{
  public void initFilter( final String conf, final IObservation obs );
}
