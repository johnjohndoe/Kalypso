package org.kalypso.ogc.sensor;

/**
 * Default implementation of the IObservationProvider interface.
 * 
 * @author schlienger
 */
public class DefaultObservationProvider implements IObservationProvider
{
  private final IObservation m_obs;
  private final IAxis m_sharedAxis;
  private final IAxis m_valueAxis;

  public DefaultObservationProvider( final IObservation obs, final IAxis sharedAxis, final IAxis valueAxis )
  {
    m_obs = obs;
    m_sharedAxis = sharedAxis;
    m_valueAxis = valueAxis;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationProvider#getObservation()
   */
  public IObservation getObservation()
  {
    return m_obs;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationProvider#getSharedAxis()
   */
  public IAxis getSharedAxis()
  {
    return m_sharedAxis;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationProvider#getValueAxis()
   */
  public IAxis getValueAxis()
  {
    return m_valueAxis;
  }
}
