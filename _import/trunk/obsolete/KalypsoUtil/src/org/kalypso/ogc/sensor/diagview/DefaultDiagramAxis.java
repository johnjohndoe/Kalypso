package org.kalypso.ogc.sensor.diagview;

import org.kalypso.template.obsdiagview.TypeAxis;

/**
 * Default implementation of <code>IDiagramAxis</code>.
 * 
 * @author schlienger
 */
public class DefaultDiagramAxis implements IDiagramAxis
{
  private final String m_label;
  private final String m_unit;
  private final String m_direction;
  private final String m_position;
  private final boolean m_isInverted;
  private final String m_dataType;

  public DefaultDiagramAxis( final TypeAxis axis )
  {
    this( axis.getDatatype(), axis.getLabel(), axis.getUnit(), axis.getDirection(), axis.getPosition(), axis.isInverted() );
  }
  
  public DefaultDiagramAxis( final String dataType, final String label, final String unit, final String direction, final String position, final boolean isInverted )
  {
    m_dataType = dataType;
    m_label = label;
    m_unit = unit;
    m_direction = direction;
    m_position = position;
    m_isInverted = isInverted;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramAxis#getLabel()
   */
  public String getLabel()
  {
    return m_label;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramAxis#getUnit()
   */
  public String getUnit()
  {
    return m_unit;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramAxis#getDirection()
   */
  public String getDirection()
  {
    return m_direction;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramAxis#getPosition()
   */
  public String getPosition()
  {
    return m_position;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramAxis#isInverted()
   */
  public boolean isInverted()
  {
    return m_isInverted;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramAxis#getDataType()
   */
  public String getDataType()
  {
    return m_dataType;
  }
}
