package org.kalypso.ogc.sensor.filter.creators;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.IFilterCreator;
import org.kalypso.ogc.sensor.filter.IObservationFilter;
import org.kalypso.ogc.sensor.filter.filters.ZmlFilter;
import org.kalypso.zml.filters.AbstractFilterType;
import org.kalypso.zml.filters.ZmlFilterType;

/**
 * ZmlFilterCreator
 * 
 * @author schlienger
 */
public final class ZmlFilterCreator implements IFilterCreator
{
  /**
   * @see org.kalypso.ogc.sensor.filter.IFilterCreator#createFilter(org.kalypso.zml.filters.AbstractFilterType, org.kalypso.ogc.sensor.IObservation)
   */
  public IObservationFilter createFilter( final AbstractFilterType aft,
      final IObservation obs ) throws SensorException
  {
    if( !(aft instanceof ZmlFilterType) )
      throw new IllegalArgumentException( "Not a " + ZmlFilterType.class.getName() );
    
    final ZmlFilterType ft = (ZmlFilterType) aft;

    final ZmlFilter filter = new ZmlFilter();
    filter.initFilter( ft.getZml().getHref(), obs );
    
    return filter;
  }
}
