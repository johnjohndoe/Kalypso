package org.kalypso.ogc.sensor.zml.values;

import java.util.Map;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.AbstractTuppleModel;

/**
 * A specific TuppleModel that can deal with values coming from Zml-Files.
 * 
 * @author schlienger
 */
public class ZmlTuppleModel extends AbstractTuppleModel
{
  private final Map m_valuesMap;

  /**
   * Constructor
   * 
   * @param valuesMap
   */
  public ZmlTuppleModel( final Map valuesMap )
  {
    m_valuesMap = valuesMap;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getCount()
   */
  public int getCount( ) throws SensorException
  {
    if( m_valuesMap.size() == 0 )
      return 0;

    return ((IZmlValues) m_valuesMap.values().iterator().next()).getCount();
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public int indexOf( final Object element, final IAxis axis )
      throws SensorException
  {
    if( m_valuesMap.size() == 0 )
      throw new IllegalStateException( "No Axis" );

    final IZmlValues values = (IZmlValues) m_valuesMap.get( axis );
    if( values == null )
      return -1;

    return values.indexOf( element );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getAxisList()
   */
  public IAxis[] getAxisList( )
  {
    return (IAxis[]) m_valuesMap.keySet().toArray( new IAxis[0]);
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public Object getElement( final int index, final IAxis axis )
      throws SensorException
  {
    if( m_valuesMap.size() == 0 )
      throw new IllegalStateException( "No Axis" );

    final IZmlValues values = (IZmlValues) m_valuesMap.get( axis );
    if( values == null )
      return new Double( 0 );

    return values.getElement( index );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public void setElement( final int index, final Object element,
      final IAxis axis ) throws SensorException
  {
    if( m_valuesMap.size() == 0 )
      throw new IllegalStateException( "No Axis" );

    ((IZmlValues) m_valuesMap.get( axis )).setElement( index, element );
  }
}