package org.kalypso.wiskiadapter;

import java.io.File;
import java.util.Date;

import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableSet;
import org.kalypso.util.cache.StringValidityFileCache;
import org.kalypso.util.cache.StringValidityKey;

/**
 * RatingTableCache
 * 
 * @author schlienger
 */
public class RatingTableCache
{
  private static RatingTableCache m_instance = null;

  private final StringValidityFileCache m_cache;

  private RatingTableCache( )
  {
    m_cache = new StringValidityFileCache( WQTableFactory.getInstance(),
        new File( WiskiUtils.getProperties().getProperty( "CACHE_DIRECTORY" ) ) );
  }

  public static RatingTableCache getInstance( )
  {
    if( m_instance == null )
      m_instance = new RatingTableCache();

    return m_instance;
  }

  /**
   * Tries to fetch it from the cache, if available
   * 
   * @param wiskiId
   * @param validity
   * @return null if not found in cache
   */
  public WQTableSet get( final Long wiskiId, final Date validity )
  {
    return (WQTableSet) m_cache.get( new StringValidityKey( String
        .valueOf( wiskiId ), validity ) );
  }

  /**
   * Checks if this one is more recent than the one in the cache and eventually
   * stores it in the cache
   * 
   * @param wqTableSet
   * @param wiskiId
   * @param to
   */
  public void check( final WQTableSet wqTableSet, final Long wiskiId,
      final Date to )
  {
    final StringValidityKey key = new StringValidityKey( String
        .valueOf( wiskiId ), to );

    final StringValidityKey cacheKey = (StringValidityKey) m_cache
        .getRealKey( key );

    if( cacheKey != null && cacheKey.getValidity().after( to ) )
      return; // no need to overwrite if more recent in the cache

    m_cache.addObject( key, wqTableSet );
  }
}
