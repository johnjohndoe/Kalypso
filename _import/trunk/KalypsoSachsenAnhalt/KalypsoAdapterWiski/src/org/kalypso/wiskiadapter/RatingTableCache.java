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

  private RatingTableCache()
  {
    final File dir = new File( System.getProperty( "java.io.tmpdir" )
        + File.separator + "wiskiRatingTables" );
    if( !dir.exists() )
      dir.mkdir();
    
    m_cache = new StringValidityFileCache( WQTableFactory.getInstance(), dir );
  }

  public static RatingTableCache getInstance()
  {
    if( m_instance == null )
      m_instance = new RatingTableCache();

    return m_instance;
  }

  /**
   * Tries to fetch it from the cache, if available
   * 
   * @return null if not found in cache
   */
  public WQTableSet get( final String tsInfoName, final Date validity )
  {
    return (WQTableSet)m_cache
        .get( new StringValidityKey( tsInfoName, validity ) );
  }

  /**
   * Checks if this one is more recent than the one in the cache and eventually
   * stores it in the cache
   */
  public void check( final WQTableSet wqTableSet, final String tsInfoName,
      final Date to )
  {
    final StringValidityKey key = new StringValidityKey( tsInfoName, to );

    final StringValidityKey cacheKey = m_cache.getRealKey( key );

    if( cacheKey != null && cacheKey.getValidity().after( to ) )
      return; // no need to overwrite if more recent in the cache

    m_cache.addObject( key, wqTableSet );
  }
}
