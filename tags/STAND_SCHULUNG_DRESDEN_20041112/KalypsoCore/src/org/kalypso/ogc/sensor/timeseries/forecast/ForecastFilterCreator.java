package org.kalypso.ogc.sensor.timeseries.forecast;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.IFilterCreator;
import org.kalypso.ogc.sensor.filter.IObservationFilter;
import org.kalypso.ogc.sensor.filter.creators.FilterCreatorHelper;
import org.kalypso.zml.filters.AbstractFilterType;
import org.kalypso.zml.filters.ForecastFilterType;

/**
 * MergeFilterCreator
 * 
 * @author schlienger
 */
public class ForecastFilterCreator implements IFilterCreator
{
  /**
   * @see org.kalypso.ogc.sensor.filter.IFilterCreator#createFilter(org.kalypso.zml.filters.AbstractFilterType, org.kalypso.ogc.sensor.IObservation)
   */
  public IObservationFilter createFilter( AbstractFilterType aft,
      IObservation baseObs ) throws SensorException
  {
    if( !(aft instanceof ForecastFilterType) )
      throw new IllegalArgumentException( "Not a " + ForecastFilterType.class.getName() );
    
    final ForecastFilterType ft = (ForecastFilterType) aft;

    final IObservation[] filteredObs = FilterCreatorHelper.resolveFilters( ft.getFilter(), baseObs );
	
    final ForecastFilter filter = new ForecastFilter();
    filter.initFilter( filteredObs, filteredObs[0] );
    
    return filter;
  }
}
