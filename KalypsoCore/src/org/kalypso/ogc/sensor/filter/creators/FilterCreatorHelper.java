package org.kalypso.ogc.sensor.filter.creators;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.filter.FilterFactory;
import org.kalypso.ogc.sensor.filter.IFilterCreator;
import org.kalypso.util.factory.FactoryException;
import org.kalypso.zml.filters.AbstractFilterType;

/**
 * FilterCreatorHelper
 * 
 * @author schlienger
 */
public final class FilterCreatorHelper
{
  /**
   * Resolves the filter by checking if it can be created. If not, the baseObs is returned.
   * 
   * @param aft the binding type to check
   * @param baseObs the base observation onto which the filters are applied
   * @return the filtered observation
   * @throws SensorException
   */
  public static IObservation resolveFilter( final AbstractFilterType aft, final IObservation baseObs ) throws SensorException
  {
    if( aft != null )
    {
	    final IFilterCreator creator;
	    try
	    {
	      creator = FilterFactory.getCreatorInstance( aft );
	    }
	    catch( FactoryException e )
	    {
	      e.printStackTrace();
	      throw new SensorException( e );
	    }
	    
	    // recursive filtering
	    return creator.createFilter( aft, baseObs );
    }
    
    // no subfilter
    return baseObs;
  }
}
