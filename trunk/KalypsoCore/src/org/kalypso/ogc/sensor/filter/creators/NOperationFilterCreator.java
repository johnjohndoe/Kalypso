package org.kalypso.ogc.sensor.filter.creators;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.IFilterCreator;
import org.kalypso.ogc.sensor.filter.IObservationFilter;
import org.kalypso.ogc.sensor.filter.filters.NOperationFilter;
import org.kalypso.zml.filters.AbstractFilterType;
import org.kalypso.zml.filters.NOperationFilterType;

public class NOperationFilterCreator implements IFilterCreator
{
  public IObservationFilter createFilter( AbstractFilterType aft, IObservation baseObs )
      throws SensorException
  {
    final NOperationFilterType filter = (NOperationFilterType)aft;

    final IObservation[] innerObs = FilterCreatorHelper.resolveFilters( filter.getFilter(), baseObs );

    final NOperationFilter nOperationFilter = new NOperationFilter( filter );
    nOperationFilter.initFilter( innerObs, innerObs[0] );
    return nOperationFilter;
  }
}