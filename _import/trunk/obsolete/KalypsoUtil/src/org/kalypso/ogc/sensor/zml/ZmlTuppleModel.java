package org.kalypso.ogc.sensor.zml;

import java.util.Iterator;
import java.util.List;

import org.kalypso.ogc.sensor.ITuppleModel;

/**
 * @author schlienger
 *
 */
public class ZmlTuppleModel implements ITuppleModel
{
  private final ZmlAxis[] m_axes;
  
  public ZmlTuppleModel( final List axes )
  {
    m_axes = new ZmlAxis[ axes.size() ];
    
    for( Iterator it = axes.iterator(); it.hasNext(); )
    {
      ZmlAxis axis = (ZmlAxis)it.next();
      
      m_axes[ axis.getPosition() ] = axis;
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getCount()
   */
  public int getCount()
  {
    if( m_axes.length == 0 )
      return 0;
    
    return m_axes[0].getValues().size();
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getElement(int, int)
   */
  public Object getElement( int index, int position )
  {
    if( m_axes.length == 0 )
      return null;
    
    return m_axes[ position ].getValues().get( index );
  }

  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#setElement(int, java.lang.Object, int)
   */
  public void setElement( int index, Object element, int position )
  {
    if( m_axes.length == 0 )
      return;
    
    m_axes[ position ].getValues().set( index, element );
  }
}
