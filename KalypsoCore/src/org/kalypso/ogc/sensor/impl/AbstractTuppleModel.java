package org.kalypso.ogc.sensor.impl;

import java.util.HashMap;
import java.util.Map;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IAxisRange;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;

/**
 * Provides common functionnality:
 * <ul>
 * <li>getPositionFor( IAxis )</li>
 * <li>mapAxisToPos( IAxis, int )</li>
 * <li>getRangeFor( IAxis )</li>
 * </ul>
 * 
 * @author schlienger
 */
public abstract class AbstractTuppleModel implements ITuppleModel
{
  /** maps an axis to its position in this tupple model */
  private final Map m_axes2pos = new HashMap();
  
  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getPositionFor(org.kalypso.ogc.sensor.IAxis)
   */
  public int getPositionFor( final IAxis axis ) throws SensorException
  {
    if( !m_axes2pos.containsKey( axis ) )
      throw new SensorException( "Model does not contain axis: " + axis );
    
    return ((Integer)m_axes2pos.get( axis )).intValue();
  }
  
  /**
   * Maps an axis to its position in this model
   * 
   * @param axis
   * @param pos
   */
  protected void mapAxisToPos( final IAxis axis, final int pos )
  {
    m_axes2pos.put( axis, new Integer(pos) );
  }
  
  /**
   * Clears the maps
   */
  protected void clearAxesPositions()
  {
    m_axes2pos.clear();
  }
  
  /**
   * @see org.kalypso.ogc.sensor.ITuppleModel#getRangeFor(org.kalypso.ogc.sensor.IAxis)
   */
  public IAxisRange getRangeFor( final IAxis axis ) throws SensorException
  {
    if( getCount() > 0 )
    {
      Object begin = getElement( 0, axis );
      Object end = getElement( getCount() - 1, axis );
      
      return new DefaultAxisRange( begin, end );
    }
    
    return null;
  }
}
