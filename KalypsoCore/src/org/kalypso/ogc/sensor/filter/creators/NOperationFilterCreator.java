package org.kalypso.ogc.sensor.filter.creators;

import java.util.List;

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
    final List filters = filter.getFilter();
    final IObservation[] innerObs = new IObservation[filters.size()];
    for( int i = 0; i < innerObs.length; i++ )
      innerObs[i] = FilterCreatorHelper.resolveFilter( (AbstractFilterType)filters.get( i ),
          baseObs );

    final NOperationFilter nOperationFilter = new NOperationFilter( filter );
    nOperationFilter.initFilter( innerObs, innerObs[0] );
    return nOperationFilter;
  }
}