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
   * @param isKey
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
   * 
   * @param axis
   */
  public DefaultAxis( final IAxis axis )
  {
    this( axis.getName(), axis.getType(), axis.getUnit(), axis.getDataClass(),
        axis.getPosition(), axis.isKey() );
  }

  /**
   * @see org.kalypso.ogc.sensor.IAxis#getUnit()
   */
  public String getUnit( )
  {
    return m_unit;
  }

  /**
   * @see org.kalypso.ogc.sensor.IAxis#getName()
   */
  public String getName( )
  {
    return m_label;
  }

  /**
   * @see org.kalypso.ogc.sensor.IAxis#getDataClass()
   */
  public Class getDataClass( )
  {
    return m_dataClass;
  }

  public int getPosition( )
  {
    return m_position;
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    if( getUnit().length() == 0 )
      return getName();

    return getName() + " - " + getUnit();
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  public boolean equals( Object obj )
  {
    if( !(obj instanceof IAxis) )
      return false;

    final IAxis other = (IAxis) obj;

    // TODO: bin mir gar nicht sicher ob die Position doch wichtig ist!
    // ist ziemlich schlecht die position nicht zu berücksichtigen!
    // deswegen hier dieser Versuch mit position auch als criteria
    //
    // Important: Axis' position is not relevant for two axes to be equal
    if( m_dataClass == other.getDataClass() && m_isKey == other.isKey()
        && m_label.equals( other.getName() ) && m_type.equals( other.getType() )
        && m_unit.equals( other.getUnit() ) && m_position == other.getPosition() )
      return true;

    return false;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  public int hashCode( )
  {
    final StringBuffer bf = new StringBuffer();

    // Important: Axis' position is not relevant for two axes to be equal
    // TODO Gleiche Anmerkung hier wie für equals bez. position
    bf.append( m_dataClass.getName() ).append( m_isKey ).append( m_label )
        .append( m_type ).append( m_unit ).append( m_position );

    return bf.toString().hashCode();
  }

  /**
   * @see org.kalypso.ogc.sensor.IAxis#getType()
   */
  public String getType( )
  {
    return m_type;
  }

  /**
   * @see org.kalypso.ogc.sensor.IAxis#isKey()
   */
  public boolean isKey( )
  {
    return m_isKey;
  }

  public void setDataClass( Class dataClass )
  {
    m_dataClass = dataClass;
  }
}