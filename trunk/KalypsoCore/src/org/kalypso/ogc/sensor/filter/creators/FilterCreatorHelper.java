package org.kalypso.ogc.sensor.filter.creators;

import java.util.List;

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
  
  /**
   * Same as <code>FilterCreatorHelper.resolveFilter</code> but for n filters.
   * 
   * @param afts
   * @param baseObs
   * @return array of filtered observations
   * @throws SensorException
   */
  public static IObservation[] resolveFilters( final List afts, final IObservation baseObs ) throws SensorException
  {
    final IObservation[] obs = new IObservation[ afts.size()];
    
    for( int i = 0; i < obs.length; i++ )
      obs[i] = resolveFilter( (AbstractFilterType) afts.get( i ), baseObs );
    
    return obs;
  }
}
