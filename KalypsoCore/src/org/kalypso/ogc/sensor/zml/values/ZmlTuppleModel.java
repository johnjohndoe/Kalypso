package org.kalypso.ogc.sensor.zml.values;

import java.net.URL;
import java.util.Hashtable;
import java.util.Map;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlAxis;

/**
 * A specific TuppleModel that can deal with values coming from Zml-Files.
 * 
 * @author schlienger
 */
public class ZmlTuppleModel implements ITuppleModel
{
  private final ZmlAxis[] m_axes;

  private final Map m_map = new Hashtable();

  /**
   * Constructor. Fetches the values for each of the axes.
   * @param context the url of the ZmlObservation document used as context for building url with relative path 
   */
  public ZmlTuppleModel( final URL context, final ZmlAxis[] axes ) throws SensorException
  {
    m_axes = axes;
    
    for( int i = 0; i < m_axes.length; i++ )
      m_axes[i].fetchValues( context, this );
  }

  protected Object getPoolObject( Object key )
  {
    return m_map.get( key );
  }

  protected void putPoolObject( Object key, Object value )
  {
    m_map.put( key, value );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getCount()
   */
  public int getCount()
  {
    if( m_axes.length == 0 )
      return 0;

    return m_axes[0].getValues().getCount();
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int, int)
   */
  public Object getElement( int index, int position )
  {
    if( m_axes.length == 0 )
      throw new IllegalArgumentException( "No Axis" );

    return m_axes[position].getValues().getElement( index );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object,
   *      int)
   */
  public void setElement( int index, Object element, int position )
  {
    if( m_axes.length == 0 )
      throw new IllegalArgumentException( "No Axis" );
    
    m_axes[position].getValues().setElement( index, element );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#indexOf(java.lang.Object, org.kalypso.ogc.sensor.IAxis)
   */
  public int indexOf( Object element, IAxis axis )
  {
    if( m_axes.length == 0 )
      throw new IllegalArgumentException( "No Axis" );
    
    return m_axes[ axis.getPosition() ].getValues().indexOf( element );
  }
}
