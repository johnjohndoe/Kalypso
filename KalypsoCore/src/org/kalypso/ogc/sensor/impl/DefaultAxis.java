package org.kalypso.ogc.sensor.impl;

import org.kalypso.ogc.sensor.IAxis;

/**
 * Simple default implementation of the IAxis interface.
 * 
 * @author schlienger
 */
public class DefaultAxis implements IAxis
{
  private String m_label;

  private String m_unit;

  private Class m_dataClass;

  private int m_position;

  private String m_type;

  private boolean m_isKey;

  /**
   * Constructor
   * 
   * @param label
   *          label of the axis
   * @param type
   *          type of the axis
   * @param unit
   *          unit of the axis
   * @param dataClass
   *          className of the data on this axis
   * @param position
   *          position of this axis in regards to the tupple of data
   */
  public DefaultAxis( final String label, final String type, final String unit,
      final Class dataClass, final int position, final boolean isKey )
  {
    m_label = label;
    m_type = type;
    m_unit = unit;
    m_dataClass = dataClass;
    m_position = position;
    m_isKey = isKey;
  }

  /**
   * Copy Constuctor.
   */
  public DefaultAxis( final IAxis axis )
  {
    this( axis.getLabel(), axis.getType(), axis.getUnit(), axis.getDataClass(), axis.getPosition(), axis.isKey() );
  }

  /**
   * @see org.kalypso.ogc.sensor.IAxis#getUnit()
   */
  public String getUnit()
  {
    return m_unit;
  }

  /**
   * @see org.kalypso.ogc.sensor.IAxis#getLabel()
   */
  public String getLabel()
  {
    return m_label;
  }

  /**
   * @see org.kalypso.ogc.sensor.IAxis#getDataClass()
   */
  public Class getDataClass()
  {
    return m_dataClass;
  }

  public int getPosition()
  {
    return m_position;
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    if( getUnit().length() == 0 )
      return getLabel();

    return getLabel() + " - " + getUnit();
  }

  /**
   * @see org.kalypso.ogc.sensor.IAxis#getType()
   */
  public String getType()
  {
    return m_type;
  }
  
  /**
   * @see org.kalypso.ogc.sensor.IAxis#isKey()
   */
  public boolean isKey()
  {
    return m_isKey;
  }
  
  public void setDataClass( Class dataClass )
  {
    m_dataClass = dataClass;
  }
}