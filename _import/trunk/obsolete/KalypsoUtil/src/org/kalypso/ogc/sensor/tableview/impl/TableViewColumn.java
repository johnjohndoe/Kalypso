package org.kalypso.ogc.sensor.tableview.impl;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;

/**
 * @author schlienger
 */
public class TableViewColumn implements ITableViewColumn
{
  private String m_name;
  private boolean m_isEditable;
  private int m_width;
  private String m_sharedAxisName;
  private String m_valueAxisName;
  private IObservation m_obs;

  public TableViewColumn( final String name, final IObservation obs, final boolean isEditable, final int width, final String sharedAxisName, final String valueAxisName )
  {
    m_name = name;
    m_obs = obs;
    m_isEditable = isEditable;
    m_width = width;
    m_sharedAxisName = sharedAxisName;
    m_valueAxisName = valueAxisName;
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
    return m_isEditable;
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
   * @see org.kalypso.ogc.sensor.tableview.ITableObservationProvider#getSharedAxis()
   */
  public IAxis getSharedAxis()
  {
    return ObservationUtilities.findAxis( m_obs, m_sharedAxisName );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableObservationProvider#getValueAxis()
   */
  public IAxis getValueAxis()
  {
    return ObservationUtilities.findAxis( m_obs, m_valueAxisName );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationProvider#getObservation()
   */
  public IObservation getObservation()
  {
    return m_obs;
  }
}
