package org.kalypso.ogc.sensor.tableview.impl;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * @author schlienger
 */
public class DefaultTableViewColumn implements ITableViewColumn
{
  private String m_name;

  private boolean m_isEditable;

  private int m_width;

  private String m_sharedAxisName;

  private String m_valueAxisName;

  private IObservation m_obs;

  private IVariableArguments m_args = null;

  private boolean m_dirty;

  public DefaultTableViewColumn( final String name, final IObservation obs, final boolean isEditable,
      final int width, final String sharedAxisName, final String valueAxisName,
      final IVariableArguments args )
  {
    m_name = name;
    m_obs = obs;
    m_isEditable = isEditable;
    m_width = width;
    m_sharedAxisName = sharedAxisName;
    m_valueAxisName = valueAxisName;
    m_args = args;
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
    return ObservationUtilities.findAxisByName( m_obs.getAxisList(), m_sharedAxisName );
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableObservationProvider#getValueAxis()
   */
  public IAxis getValueAxis()
  {
    return ObservationUtilities.findAxisByName( m_obs.getAxisList(), m_valueAxisName );
  }

  /**
   * @see org.kalypso.ogc.sensor.IObservationProvider#getObservation()
   */
  public IObservation getObservation()
  {
    return m_obs;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#setArguments(org.kalypso.util.runtime.IVariableArguments)
   */
  public void setArguments( IVariableArguments args )
  {
    m_args = args;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getArguments()
   */
  public IVariableArguments getArguments()
  {
    return m_args;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#dispose()
   */
  public void dispose( )
  {
    // empty
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
}