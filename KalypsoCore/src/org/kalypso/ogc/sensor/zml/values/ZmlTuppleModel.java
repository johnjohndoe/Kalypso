package org.kalypso.ogc.sensor.zml.values;

import java.util.Map;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;

/**
 * A specific TuppleModel that can deal with values coming from Zml-Files.
 * 
 * @author schlienger
 */
public class ZmlTuppleModel implements ITuppleModel
{
  private final IAxis[] m_axes;

  private final Map m_valuesMap;

  /**
   * Constructor
   */
  public ZmlTuppleModel( final IAxis[] axes, final Map valuesMap )
  {
    m_axes = axes;
    m_valuesMap = valuesMap;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getCount()
   */
  public int getCount() throws SensorException
  {
    if( m_axes.length == 0 )
      return 0;

    return ( (IZmlValues)m_valuesMap.get( m_axes[0] ) ).getCount();
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public int indexOf( final Object element, final IAxis axis ) throws SensorException
  {
    if( m_axes.length == 0 )
      throw new IllegalStateException( "No Axis" );

    return ( (IZmlValues)m_valuesMap.get( axis ) ).indexOf( element );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getAxisList()
   */
  public IAxis[] getAxisList()
  {
    return m_axes;
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public Object getElement( final int index, final IAxis axis ) throws SensorException
  {
    if( m_axes.length == 0 )
      throw new IllegalStateException( "No Axis" );

    return ( (IZmlValues)m_valuesMap.get( axis ) ).getElement( index );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object,
   *      org.kalypso.ogc.sensor.IAxis)
   */
  public void setElement( final int index, final Object element, final IAxis axis )
      throws SensorException
  {
    if( m_axes.length == 0 )
      throw new IllegalStateException( "No Axis" );

    ( (IZmlValues)m_valuesMap.get( axis ) ).setElement( index, element );
  }
}