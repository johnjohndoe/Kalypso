package org.kalypso.ogc.sensor.diagview;

import org.kalypso.ogc.sensor.IObservation;


/**
 * A default curve for an observation axis.
 * 
 * @author schlienger
 *
 */
public class DefaultCurve implements ICurve
{
  private final IAxisMapping[] m_mappings;
  private final String m_name;
  private final IObservation m_obs;

  public DefaultCurve( final String name, final IObservation obs, final IAxisMapping[] mappings )
  {
    m_name = name;
    m_obs = obs;
    m_mappings = mappings;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.ICurve#getName()
   */
  public String getName()
  {
    return m_name;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.ICurve#getMappings()
   */
  public IAxisMapping[] getMappings()
  {
    return m_mappings;
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationProvider#getObservation()
   */
  public IObservation getObservation()
  {
    return m_obs;
  }
}
