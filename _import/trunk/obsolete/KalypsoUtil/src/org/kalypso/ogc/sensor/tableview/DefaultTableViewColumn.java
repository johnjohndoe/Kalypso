package org.kalypso.ogc.sensor.tableview;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;

/**
 * A simple implementation of the ITableViewColumn interface.
 * 
 * @author schlienger
 */
public class DefaultTableViewColumn implements ITableViewColumn
{
  private final String m_name;

  private final IObservation m_obs;
  private final IAxis m_sharedAxis;
  private final IAxis m_valueAxis;
  
  private boolean m_editable = true;
  private int m_width = 100;

  public DefaultTableViewColumn( final String name, final IObservation obs, final IAxis sharedAxis, final IAxis valueAxis )
  {
    m_name = name;
    m_obs = obs;
    m_sharedAxis = sharedAxis;
    m_valueAxis = valueAxis;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getName()
   */
  public String getName()
  {
    return m_name;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#isEditable()
   */
  public boolean isEditable()
  {
    return m_editable;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getWidth()
   */
  public int getWidth()
  {
    return m_width;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#setWidth(int)
   */
  public void setWidth( int width )
  {
    m_width = width;
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
