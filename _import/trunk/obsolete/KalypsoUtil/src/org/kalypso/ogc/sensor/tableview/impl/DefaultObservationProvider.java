package org.kalypso.ogc.sensor.tableview.impl;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.tableview.ITableObservationProvider;

/**
 * Default implementation of the IObservationProvider interface.
 * 
 * @author schlienger
 */
public class DefaultObservationProvider implements ITableObservationProvider
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
   * @see org.kalypso.ogc.sensor.tableview.ITableObservationProvider#getSharedAxis()
   */
  public IAxis getSharedAxis()
  {
    return m_sharedAxis;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableObservationProvider#getValueAxis()
   */
  public IAxis getValueAxis()
  {
    return m_valueAxis;
  }
}
