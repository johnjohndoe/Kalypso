package org.kalypso.ogc.sensor.timeseries;

import org.kalypso.ogc.sensor.IObservation;

/**
 * TODO
 * 
 * @author schlienger
 */
public class WQObservation implements IObservation
{
  private IObservation m_obs;

  public WQObservation( final IObservation obs )
  {
    m_obs = obs;
  }
}
