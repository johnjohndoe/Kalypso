package org.kalypso.ogc.sensor.tableview.impl;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;

/**
 * Default implementation of the <code>ITableViewColumn</code> interface
 * 
 * @author schlienger
 */
public class DefaultTableViewColumn implements ITableViewColumn
{
  private String m_name;

  private boolean m_isEditable;

  private int m_width;

  private boolean m_dirty;

  private IObservation m_obs;

  private String m_axisName;

  public DefaultTableViewColumn( final String name, final boolean isEditable,
      final int width, final String axisName, final IObservation obs )
  {
    m_name = name;
    m_isEditable = isEditable;
    m_width = width;
    m_axisName = axisName;
    m_obs = obs;
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
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#isDirty()
   */
  public boolean isDirty( )
  {
    return m_dirty;
  }
  
  /**
   * @param dirty The dirty to set.
   */
  public void setDirty( boolean dirty )
  {
    m_dirty = dirty;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getObservation()
   */
  public IObservation getObservation( )
  {
    return m_obs;
  }

  /**
   * @param observation The obs to set.
   */
  public void setObservation( final IObservation observation )
  {
    m_obs = observation;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getAxisName()
   */
  public String getAxisName( )
  {
    return m_axisName;
  }
}