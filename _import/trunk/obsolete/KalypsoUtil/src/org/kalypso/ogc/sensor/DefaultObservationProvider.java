package org.kalypso.ogc.sensor;

/**
 * Default implementation of the IObservationProvider interface.
 * 
 * @author schlienger
 */
public class DefaultObservationProvider implements IObservationProvider
{
  private final IObservation m_obs;
  private final IAxis m_axis;

  public DefaultObservationProvider( final IObservation obs, final IAxis axis )
  {
    m_obs = obs;
    m_axis = axis;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationProvider#getObservation()
   */
  public IObservation getObservation()
  {
    return m_obs;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationProvider#getDisplayAxis()
   */
  public IAxis getDisplayAxis()
  {
    return m_axis;
  }
}
