package org.kalypso.ogc.sensor.filter.creators;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.IFilterCreator;
import org.kalypso.ogc.sensor.filter.IObservationFilter;
import org.kalypso.ogc.sensor.filter.filters.OperationFilter;
import org.kalypso.zml.filters.AbstractFilterType;
import org.kalypso.zml.filters.OperationFilterType;

public class OperationFilterCreator implements IFilterCreator
{
  public IObservationFilter createFilter( AbstractFilterType aft, IObservation baseObs )
      throws SensorException
  {
    OperationFilterType filter = (OperationFilterType)aft;
    OperationFilter operationFilter = new OperationFilter( filter );

    final IObservation filteredObs = FilterCreatorHelper
        .resolveFilter( filter.getFilter(), baseObs );
    operationFilter.initFilter( filteredObs, filteredObs );
    return operationFilter;
  }

}