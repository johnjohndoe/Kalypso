package org.kalypso.ogc.sensor.tableview;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;

/**
 * @author schlienger
 *
 */
public class DefaultTableViewColumn implements ITableViewColumn
{
  private final String m_name;
  private final IObservation m_obs;
  private final IAxis m_axis;
  
  private boolean m_editable = true;
  private int m_width = 100;

  public DefaultTableViewColumn( final String name, final IObservation obs, final IAxis axis )
  {
    m_name = name;
    m_obs = obs;
    m_axis = axis;
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
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getObservation()
   */
  public IObservation getObservation()
  {
    return m_obs;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getAxis()
   */
  public IAxis getAxis()
  {
    return m_axis;
  }
}
