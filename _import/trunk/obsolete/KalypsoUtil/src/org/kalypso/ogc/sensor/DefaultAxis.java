package org.kalypso.ogc.sensor;

import java.util.List;


/**
 * Simple default implementation of the IAxis interface.
 * 
 * @author schlienger
 */
public class DefaultAxis implements IAxis
{
  private final String m_label;
  private final String m_unit;
  private final boolean m_restricted;
  private final Class m_dataClass;
  private final int m_position;
  private List m_restrictedValues;

  /**
   * Constructor
   * 
   * @param label label of the axis
   * @param unit unit of the axis
   * @param dataClass className of the data on this axis
   * @param restricted true if axis restricts the range of data that can be assigned
   * @param position position of this axis in regards to the tupple of data
   */
  public DefaultAxis( final String label, final String unit, final Class dataClass, final boolean restricted, final int position )
  {
    m_label = label;
    m_unit = unit;
    m_dataClass = dataClass;
    m_restricted = restricted;
    m_position = position;
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
    return getLabel() + " - " + getUnit();
  }
}
