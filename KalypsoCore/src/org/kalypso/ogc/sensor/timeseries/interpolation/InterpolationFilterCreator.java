package org.kalypso.ogc.sensor.timeseries.interpolation;

import org.kalypso.java.util.CalendarUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.IFilterCreator;
import org.kalypso.ogc.sensor.filter.IObservationFilter;
import org.kalypso.ogc.sensor.filter.creators.FilterCreatorHelper;
import org.kalypso.zml.filters.AbstractFilterType;
import org.kalypso.zml.filters.InterpolationFilterType;

/**
 * InterpolationFilterCreator
 * 
 * @author schlienger
 */
public class InterpolationFilterCreator implements IFilterCreator
{
  /**
   * @see org.kalypso.ogc.sensor.filter.IFilterCreator#createFilter(org.kalypso.zml.filters.AbstractFilterType,
   *      org.kalypso.ogc.sensor.IObservation)
   */
  public IObservationFilter createFilter( AbstractFilterType aft,
      IObservation baseObs ) throws SensorException
  {
    if( !(aft instanceof InterpolationFilterType) )
      throw new IllegalArgumentException( "Not a "
          + InterpolationFilterType.class.getName() );

    final InterpolationFilterType ft = (InterpolationFilterType) aft;

    final IObservation filteredObs = FilterCreatorHelper.resolveFilter( ft
        .getFilter(), baseObs );

    final InterpolationFilter filter = new InterpolationFilter( CalendarUtilities.getCalendarField( ft.getCalendarField() ), ft.getAmount(), ft.isForceFill(), ft.getDefaultValue(), ft.getDefaultStatus() );
    filter.initFilter( null, filteredObs );

    return filter;
  }
}