package org.kalypso.ogc.sensor.tableview.impl;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * Default implementation of the <code>ITableViewColumn</code> interface
 * 
 * @author schlienger
 */
public class DefaultTableViewColumn implements ITableViewColumn
{
  private String m_name = "";

  private boolean m_isEditable = true;

  private int m_width = 50;

  private boolean m_dirty = false;

  private IObservation m_obs = null;

  private String m_axisName = "";

  private IAxis m_keyAxis = null;

  private IAxis m_valueAxis = null;

  private IVariableArguments m_args;

  /**
   * Constructor
   * 
   * @param name
   * @param isEditable
   * @param width
   * @param axisName
   * @param obs [optional] setObservation( obs ) can be called later to set the observation
   */
  public DefaultTableViewColumn( final String name, final boolean isEditable,
      final int width, final String axisName, final IObservation obs )
  {
    m_name = name;
    m_isEditable = isEditable;
    m_width = width;
    m_axisName = axisName;
    setObservation( obs );
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
   * Method called from constructor so is declared final.
   * 
   * @param observation The obs to set.
   */
  public final void setObservation( final IObservation observation )
  {
    m_obs = observation;

    // reset
    m_keyAxis = null;
    m_valueAxis = null;

    if( m_obs != null )
    {
      final IAxis[] axes = m_obs.getAxisList();

      final IAxis[] keys = ObservationUtilities.findAxisByKey( axes );
      if( keys.length != 0 )
        m_keyAxis = keys[0];
      
      m_valueAxis = ObservationUtilities.findAxisByName( axes, m_axisName );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getColumnClass()
   */
  public Class getColumnClass( )
  {
    if( m_valueAxis != null )
      return m_valueAxis.getDataClass();
    
    return Object.class;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getAxis()
   */
  public IAxis getAxis( )
  {
    return m_valueAxis;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getKeyAxis()
   */
  public IAxis getKeyAxis( )
  {
    return m_keyAxis;
  }

  /**
   * @param args
   */
  public void setArguments( IVariableArguments args )
  {
    m_args = args;
  }
  
  public IVariableArguments getArguments( )
  {
    return m_args;
  }
}