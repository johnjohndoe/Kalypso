package org.kalypso.ogc.sensor.impl;

import java.util.List;

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

  protected boolean m_restricted;

  protected Class m_dataClass;

  protected int m_position;

  protected List m_restrictedValues;

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
   * @param restricted
   *          true if axis restricts the range of data that can be assigned
   * @param position
   *          position of this axis in regards to the tupple of data
   */
  public DefaultAxis( final String label, final String type, final String unit,
      final Class dataClass, final boolean restricted, final int position )
  {
    m_label = label;
    m_type = type;
    m_unit = unit;
    m_dataClass = dataClass;
    m_restricted = restricted;
    m_position = position;
  }

  /**
   * Copy Constuctor.
   */
  public DefaultAxis( final IAxis axis )
  {
    this( axis.getLabel(), axis.getType(), axis.getUnit(), axis.getDataClass(),
        axis.isRestricted(), axis.getPosition() );

    setRestrictedValues( axis.getRestrictedValues() );
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

  public List getRestrictedValues()
  {
    return m_restrictedValues;
  }

  public void setRestrictedValues( List restrictedValues )
  {
    m_restrictedValues = restrictedValues;
  }

  public boolean isRestricted()
  {
    return m_restricted;
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