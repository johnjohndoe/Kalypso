package org.kalypso.ogc.sensor.timeseries.wq;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.IFilterCreator;
import org.kalypso.ogc.sensor.filter.IObservationFilter;
import org.kalypso.ogc.sensor.filter.creators.FilterCreatorHelper;
import org.kalypso.zml.filters.AbstractFilterType;
import org.kalypso.zml.filters.WqFilterType;

/**
 * WQFilterCreator
 * 
 * @author schlienger
 */
public final class WQFilterCreator implements IFilterCreator
{
  /**
   * @see org.kalypso.ogc.sensor.filter.IFilterCreator#createFilter(org.kalypso.zml.filters.AbstractFilterType, org.kalypso.ogc.sensor.IObservation)
   */
  public IObservationFilter createFilter( final AbstractFilterType aft, final IObservation baseObs )
      throws SensorException
  {
    if( !(aft instanceof WqFilterType) )
      throw new IllegalArgumentException( "Not a " + WqFilterType.class.getName() );
    
    final WqFilterType wqft = (WqFilterType) aft;

    final IObservation filteredObs = FilterCreatorHelper.resolveFilter( wqft.getFilter(), baseObs );
	
    final WQObservationFilter filter = new WQObservationFilter();
    filter.initFilter( wqft.getType(), filteredObs );
    
    return filter;
  }
}
