package org.kalypso.ogc.sensor;


/**
 * Simple default abstract implementation of the IAxis interface. Can be used as base 
 * class for specific axis implementation.
 * 
 * @author schlienger
 */
public abstract class AbstractAxis implements IAxis
{
  private final String m_label;
  private final String m_unit;

  public AbstractAxis( final String label, final String unit )
  {
    m_label = label;
    m_unit = unit;
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
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    return getLabel() + " - " + getUnit();
  }
}
