package org.kalypso.ogc.sensor.view;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.util.adapter.IAdaptable;
import org.shiftone.cache.Cache;
import org.shiftone.cache.policy.lfu.LfuCacheFactory;

/**
 * A very simple cache for observations...
 * 
 * @author schlienger
 */
public class ObservationCache
{
  /** timeout of 5 minutes */
  private final static int timeout = 1000 * 60 * 5;

  /** cache size of 200 */
  private final static int size = 200;

  /** our cache */
  private static Cache m_cache = null;

  private ObservationCache()
  {
  // not intended to be instanciated
  }

  private static Cache getCache()
  {
    if( m_cache == null )
    {
      final LfuCacheFactory factory = new LfuCacheFactory();
      m_cache = factory.newInstance( "view.observations", timeout, size );
    }

    return m_cache;
  }

  public static IObservation getObservationFor( final IAdaptable adapt )
  {
    synchronized( getCache() )
    {
      IObservation obs = (IObservation)getCache().getObject( adapt );

      if( obs == null )
      {
        obs = (IObservation)adapt.getAdapter( IObservation.class );

        getCache().addObject( adapt, obs );
      }

      return obs;
    }
  }

  public static void dispose()
  {
    synchronized( getCache() )
    {
      getCache().clear();
    }
  }
}