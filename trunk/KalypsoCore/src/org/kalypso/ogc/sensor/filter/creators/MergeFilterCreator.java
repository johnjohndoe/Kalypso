package org.kalypso.ogc.sensor.filter.creators;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.IFilterCreator;
import org.kalypso.ogc.sensor.filter.IObservationFilter;
import org.kalypso.zml.filters.AbstractFilterType;

/**
 * MergeFilterCreator
 * 
 * @author schlienger
 */
public class MergeFilterCreator implements IFilterCreator
{

  /**
   * 
   */
  public MergeFilterCreator( )
  {
    super();
    // TODO Auto-generated constructor stub
  }

  /**
   * @see org.kalypso.ogc.sensor.filter.IFilterCreator#createFilter(org.kalypso.zml.filters.AbstractFilterType, org.kalypso.ogc.sensor.IObservation)
   */
  public IObservationFilter createFilter( AbstractFilterType aft,
      IObservation baseObs ) throws SensorException
  {
    // TODO Auto-generated method stub
    return null;
  }

}
