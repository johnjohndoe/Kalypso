package org.kalypso.ogc.sensor.filter;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.zml.filters.AbstractFilterType;

/**
 * IFilterCreator
 * 
 * @author schlienger
 */
public interface IFilterCreator
{
  /**
   * Creates the observation filter
   * 
   * @param aft
   *          the binding object from which to create the FilterCreator
   * @param baseObs
   *          [optional] the observation on which the filtering will be done.
   *          This argument is optional. Basically there are two possibilities
   *          to define the specification of a filter:
   *          <nl>
   *          <li>using the URL to link to an observation. In that case
   *          <code>obs</code> must be specified when calling this method
   *          <li>using the full xml (in the case of the virtual repository)
   *          where the observation is coded as a filter (link to a real
   *          observation). In that case you won't set <code>obs</code> here
   *          since it is fetched per se.
   *          </nl>
   * 
   * @return filter
   * @throws SensorException
   */
  public IObservationFilter createFilter( AbstractFilterType aft,
      IObservation baseObs ) throws SensorException;
}