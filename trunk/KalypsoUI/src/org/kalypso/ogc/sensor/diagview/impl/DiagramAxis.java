package org.kalypso.ogc.sensor.diagview.impl;

import org.kalypso.ogc.sensor.diagview.IDiagramAxis;
import org.kalypso.template.obsdiagview.TypeAxis;

/**
 * Default implementation of <code>IDiagramAxis</code>. This class overrides
 * equals and hashcode. Two instance of IDiagramAxis are considered to be equal
 * if they have the same id.
 * 
 * @author schlienger
 */
public class DiagramAxis implements IDiagramAxis
{
  private final String m_id;

  private final String m_label;

  private final String m_unit;

  private final String m_direction;

  private final String m_position;

  private final boolean m_isInverted;

  private final String m_dataType;

  private final Double m_lowerMargin;

  private final Double m_upperMargin;

  public DiagramAxis( final TypeAxis axis )
  {
    // TODO currently the binding says that lower and upper margins
    // are optional, but we could not find out any solution 
    // to know if the values were specified or not (primivite types, 
    // for Object types it's not a problem since value is null)
    // So as soon as solution is found, maybe replace the code
    // here that currently simply checks for == 0
    this(
        axis.getId(),
        axis.getDatatype(),
        axis.getLabel(),
        axis.getUnit(),
        axis.getDirection(),
        axis.getPosition(),
        axis.isInverted(),
        axis.getLowerMargin() == 0 ? null : new Double( axis.getLowerMargin() ),
        axis.getUpperMargin() == 0 ? null : new Double( axis.getUpperMargin() ) );
  }

  public DiagramAxis( final String id, final String dataType,
      final String label, final String unit, final String direction,
      final String position, final boolean isInverted )
  {
    this( id, dataType, label, unit, direction, position, isInverted, null,
        null );
  }

  public DiagramAxis( final String id, final String dataType,
      final String label, final String unit, final String direction,
      final String position, final boolean isInverted,
      final Double lowerMargin, final Double upperMargin )
  {
    m_id = id;
    m_dataType = dataType;
    m_label = label;
    m_unit = unit;
    m_direction = direction;
    m_position = position;
    m_isInverted = isInverted;
    m_lowerMargin = lowerMargin;
    m_upperMargin = upperMargin;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramAxis#getLabel()
   */
  public String getLabel( )
  {
    return m_label;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramAxis#getUnit()
   */
  public String getUnit( )
  {
    return m_unit;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramAxis#getDirection()
   */
  public String getDirection( )
  {
    return m_direction;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramAxis#getPosition()
   */
  public String getPosition( )
  {
    return m_position;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramAxis#isInverted()
   */
  public boolean isInverted( )
  {
    return m_isInverted;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramAxis#getDataType()
   */
  public String getDataType( )
  {
    return m_dataType;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramAxis#getIdentifier()
   */
  public String getIdentifier( )
  {
    return m_id;
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    return toFullString();
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  public boolean equals( Object obj )
  {
    if( !(obj instanceof IDiagramAxis) )
      return false;

    IDiagramAxis other = (IDiagramAxis) obj;

    return m_id.equals( other.getIdentifier() );
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  public int hashCode( )
  {
    return m_id.hashCode();
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramAxis#toFullString()
   */
  public String toFullString( )
  {
    if( m_unit.length() > 0 )
      return m_label + " [" + m_unit + "]";

    return m_label;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramAxis#getLowerMargin()
   */
  public Double getLowerMargin( )
  {
    return m_lowerMargin;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IDiagramAxis#getUpperMaring()
   */
  public Double getUpperMaring( )
  {
    return m_upperMargin;
  }
}