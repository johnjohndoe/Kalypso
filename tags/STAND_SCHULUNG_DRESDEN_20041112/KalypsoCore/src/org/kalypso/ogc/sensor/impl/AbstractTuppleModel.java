package org.kalypso.ogc.sensor.impl;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IAxisRange;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;

/**
 * Provides common functionnality:
 * <ul>
 * <li>getRangeFor( IAxis )
 * </ul>
 * 
 * @author schlienger
 */
public abstract class AbstractTuppleModel implements ITuppleModel
{
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
