package org.kalypso.services.ocs.repository;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.shiftone.cache.Cache;
import org.shiftone.cache.policy.lfu.LfuCacheFactory;

/**
 * A very simple cache for values of observations...
 * 
 * @author schlienger
 */
public class ObservationValuesCache
{
  /** timeout of 4 mins */
  private final static int timeout = 1000 * 60 * 4;

  /** cache size of 200 */
  private final static int size = 200;

  /** our cache */
  private static Cache m_cache = null;

  private ObservationValuesCache(  )
  {
  // not intended to be instanciated
  }

  private static Cache getCache()
  {
    if( m_cache == null )
    {
      final LfuCacheFactory factory = new LfuCacheFactory();
      m_cache = factory.newInstance( "view.observations.values", timeout, size );
    }

    return m_cache;
  }

  
  public static ITuppleModel getValues( final IObservation obs )
  {
    synchronized( getCache() )
    {
      return (ITuppleModel) getCache().getObject( obs );
    }
  }
  

  public static void addValues( final IObservation obs, final ITuppleModel values )
  {
    synchronized( getCache() )
    {
      getCache().addObject( obs, values );
    }
  }

  
  public static void clear()
  {
    synchronized( getCache() )
    {
      getCache().clear();
    }
  }
}