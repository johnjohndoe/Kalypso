package org.kalypso.ogc.sensor.impl;

import org.kalypso.ogc.sensor.IAxis;

/**
 * Simple default implementation of the IAxis interface.
 * 
 * @author schlienger
 */
public class DefaultAxis implements IAxis
{
  protected String m_label;

  protected String m_unit;

  protected Class m_dataClass;

  protected int m_position;

  protected String m_type;

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
      final Class dataClass, final int position )
  {
    m_label = label;
    m_type = type;
    m_unit = unit;
    m_dataClass = dataClass;
    m_position = position;
  }

  /**
   * Copy Constuctor.
   */
  public DefaultAxis( final IAxis axis )
  {
    this( axis.getLabel(), axis.getType(), axis.getUnit(), axis.getDataClass(), axis.getPosition() );
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
}