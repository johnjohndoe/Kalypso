package org.kalypso.ogc.sensor.tableview.impl;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewTheme;

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

  private final IAxis m_keyAxis;

  private final IAxis m_valueAxis;

  private ITableViewTheme m_theme;

  private boolean m_dirty = false;

  /**
   * Constructor
   * 
   * @param name
   * @param isEditable
   * @param width
   * @param keyAxis
   * @param valueAxis
   * @param theme
   */
  public DefaultTableViewColumn( final String name, final boolean isEditable,
      final int width, final IAxis keyAxis, final IAxis valueAxis,
      final ITableViewTheme theme )
  {
    m_name = name;
    m_isEditable = isEditable;
    m_width = width;
    m_keyAxis = keyAxis;
    m_valueAxis = valueAxis;
    m_theme = theme;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getName()
   */
  public String getName( )
  {
    return m_name;
  }
  
  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    return getName();
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#isEditable()
   */
  public boolean isEditable( )
  {
    return m_isEditable;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getWidth()
   */
  public int getWidth( )
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
   * @param dirty
   *          The dirty to set.
   */
  public void setDirty( boolean dirty )
  {
    m_dirty = dirty;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getColumnClass()
   */
  public Class getColumnClass( )
  {
    return m_valueAxis.getDataClass();
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
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#getTheme()
   */
  public ITableViewTheme getTheme( )
  {
    return m_theme;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewColumn#setName(java.lang.String)
   */
  public void setName( String name )
  {
    m_name = name;
  }
}