package org.kalypso.ogc.sensor.diagview.impl;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.diagview.IDiagramAxis;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
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

  public DiagramAxis( final TypeAxis axis )
  {
    this( axis.getId(), axis.getDatatype(), axis.getLabel(), axis.getUnit(),
        axis.getDirection(), axis.getPosition(), axis.isInverted() );
  }

  public DiagramAxis( final String id, final String dataType,
      final String label, final String unit, final String direction,
      final String position, final boolean isInverted )
  {
    m_id = id;
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
   * Creates a diagram axis according to the given IObservation axis
   * 
   * @param axis
   * @return diagram axis
   */
  public static DiagramAxis createAxisFor( final IAxis axis )
  {
    return createAxisFor( axis.getType(), axis.getName(), axis.getUnit() );
  }

  /**
   * Creates a diagram axis according to the given IObservation axis
   * 
   * @param axisType
   * @param label
   * @param unit
   * @return diagram axis
   */
  public static DiagramAxis createAxisFor( final String axisType,
      final String label, final String unit )
  {
    if( axisType.equals( TimeserieConstants.TYPE_DATE ) )
      return new DiagramAxis( axisType, "date", label, unit,
          IDiagramAxis.DIRECTION_HORIZONTAL, IDiagramAxis.POSITION_BOTTOM,
          false );

    if( axisType.equals( TimeserieConstants.TYPE_WATERLEVEL ) )
      return new DiagramAxis( axisType, "double", label, unit,
          IDiagramAxis.DIRECTION_VERTICAL, IDiagramAxis.POSITION_LEFT, false );

    if( axisType.equals( TimeserieConstants.TYPE_RUNOFF ) )
      return new DiagramAxis( axisType, "double", label, unit,
          IDiagramAxis.DIRECTION_VERTICAL, IDiagramAxis.POSITION_LEFT, false );

    if( axisType.equals( TimeserieConstants.TYPE_RAINFALL ) )
      return new DiagramAxis( axisType, "double", label, unit,
          IDiagramAxis.DIRECTION_VERTICAL, IDiagramAxis.POSITION_RIGHT, true );

    if( axisType.equals( TimeserieConstants.TYPE_TEMPERATURE ) )
      return new DiagramAxis( axisType, "double", label, unit,
          IDiagramAxis.DIRECTION_VERTICAL, IDiagramAxis.POSITION_RIGHT, false );

    // default axis
    return new DiagramAxis( axisType, "double", label, unit,
        IDiagramAxis.DIRECTION_VERTICAL, IDiagramAxis.POSITION_LEFT, false );
  }
}