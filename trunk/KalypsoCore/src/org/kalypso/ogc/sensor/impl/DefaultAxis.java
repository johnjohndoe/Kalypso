package org.kalypso.ogc.sensor.impl;

import org.kalypso.ogc.sensor.IAxis;

/**
 * Default implementation of the IAxis interface.
 * 
 * @author schlienger
 */
public class DefaultAxis implements IAxis
{
  private final String m_label;

  private final String m_unit;

  private final Class m_dataClass;

  private final String m_type;

  private final boolean m_isKey;
  
  private final boolean m_persistable;

  /**
   * Constructor. Calls the full constructor with the persistable argument set
   * to true.
   * 
   * @param label
   * @param type
   * @param unit
   * @param dataClass
   * @param isKey
   */
  public DefaultAxis( final String label, final String type, final String unit,
      final Class dataClass, final boolean isKey )
  {
    this( label, type, unit, dataClass, isKey, true );
  }
  
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
   * @param isKey
   * @param persistable
   */
  public DefaultAxis( final String label, final String type, final String unit,
      final Class dataClass, final boolean isKey, final boolean persistable )
  {
    m_label = label;
    m_type = type;
    m_unit = unit;
    m_dataClass = dataClass;
    m_isKey = isKey;
    m_persistable = persistable;
  }

  /**
   * Copy Constuctor.
   * 
   * @param axis
   */
  public DefaultAxis( final IAxis axis )
  {
    this( axis.getName(), axis.getType(), axis.getUnit(), axis.getDataClass(),
        axis.isKey(), axis.isPersistable() );
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

    if( m_dataClass == other.getDataClass() && m_isKey == other.isKey()
        && m_type.equals( other.getType() )
        && m_unit.equals( other.getUnit() ) )
      return true;

    return false;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  public int hashCode( )
  {
    final StringBuffer bf = new StringBuffer();

    bf.append( m_dataClass.getName() ).append( m_isKey ).append( m_label )
        .append( m_type ).append( m_unit );

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

  /**
   * @see org.kalypso.ogc.sensor.IAxis#isPersistable()
   */
  public boolean isPersistable( )
  {
    return m_persistable;
  }
}