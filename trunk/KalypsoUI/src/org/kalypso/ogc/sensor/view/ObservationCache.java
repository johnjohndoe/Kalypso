package org.kalypso.ogc.sensor.view;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
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
  private static ObservationCache m_instance = null;

  public static ObservationCache getInstance( )
  {
    if( m_instance == null )
    {
      //    timeout of 4 minutes and cache size of 200
      m_instance = new ObservationCache( 1000 * 60 * 4, 200 );
    }

    return m_instance;
  }
  
  public static void clearCache()
  {
    if( m_instance != null )
      m_instance.clear();
  }

  
  /** our cache */
  private final Cache m_cache;

  public ObservationCache( final int timeout, final int size )
  {
    final LfuCacheFactory factory = new LfuCacheFactory();
    m_cache = factory.newInstance( "view.observations", timeout, size );
  }

  public IObservation getObservationFor( final IAdaptable adapt )
  {
    synchronized( m_cache )
    {
      IObservation obs = (IObservation) m_cache.getObject( adapt );

      if( obs == null )
      {
        obs = (IObservation) adapt.getAdapter( IObservation.class );

        // still null, then this item is not adaptable
        if( obs == null )
          return null;

        m_cache.addObject( adapt, obs );
      }

      return obs;
    }
  }

  public ITuppleModel getValues( final IObservation obs )
  {
    synchronized( m_cache )
    {
      return (ITuppleModel) m_cache.getObject( obs );
    }
  }

  public void addValues( final IObservation obs, final ITuppleModel values )
  {
    synchronized( m_cache )
    {
      m_cache.addObject( obs, values );
    }
  }

  public void clear( )
  {
    synchronized( m_cache )
    {
      m_cache.clear();
    }
  }
}