package org.kalypso.ogc.sensor.filter.filters;

import java.util.List;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.filters.valuecomp.IValueComp;

/**
 * ValueFilter
 * 
 * @author schlienger
 */
public class ValueFilter extends AbstractObservationFilter
{
  /**
   * @see org.kalypso.ogc.sensor.filter.filters.AbstractObservationFilter#initFilter(java.lang.Object,
   *      org.kalypso.ogc.sensor.IObservation)
   */
  public void initFilter( Object conf, IObservation obs ) throws SensorException
  {
    super.initFilter( conf, obs );
    
    final IValueComp[] comps = (IValueComp[]) ((List)conf).toArray( new IValueComp[0]);
    
    // TODO implement it...
  }
}