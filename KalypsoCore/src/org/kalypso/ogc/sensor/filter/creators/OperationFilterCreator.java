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
    /*
     * 
     * @see org.kalypso.ogc.sensor.filter.IFilterCreator#createFilter(org.kalypso.zml.filters.AbstractFilterType,
     *      org.kalypso.ogc.sensor.IObservation)
     */
    public IObservationFilter createFilter(AbstractFilterType aft,
            IObservation baseObs) throws SensorException
    {

        OperationFilterType filter = (OperationFilterType) aft;
        OperationFilter operationFilter = new OperationFilter();
        operationFilter.initFilter(filter, baseObs);
        return operationFilter;
    }

}