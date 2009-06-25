package org.kalypso.wiskiadapter;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.util.Date;

import org.kalypso.commons.cache.FileCache;
import org.kalypso.commons.cache.StringValidityKey;
import org.kalypso.commons.cache.StringValidityKeyFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableSet;

/**
 * RatingTableCache
 * 
 * @author schlienger
 */
public class RatingTableCache
{
  private static RatingTableCache m_instance = null;

  private final FileCache<StringValidityKey, WQTableSet> m_cache;

  private final long m_expireTime;

  /**
   * @param expireTime
   *          Time (in ms) for how long items are kept in the cache.
   */
  private RatingTableCache( final long expireTime )
  {
    m_expireTime = expireTime;

    final File dir = new File( System.getProperty( "java.io.tmpdir" ) + File.separator + "wiskiRatingTables" );
    if( !dir.exists() )
      dir.mkdir();

    m_cache = new FileCache<StringValidityKey, WQTableSet>( new StringValidityKeyFactory(), StringValidityKey.createComparatorForStringCompareOnly(),
        WQTableFactory.getInstance(), dir );
  }

  public static RatingTableCache getInstance()
  {
    if( m_instance == null )
      m_instance = new RatingTableCache( 1000 * 60 * 60 * 24 * 7 ); // 7 days

    return m_instance;
  }

  /**
   * Tries to fetch it from the cache, if available
   * 
   * @return null if not found in cache
   */
  public WQTableSet get( final String tsInfoName, final Date validity )
  {
    try
    {
      final StringValidityKey key = new StringValidityKey( tsInfoName, validity );

      checkValidity( key );

      return m_cache.getObject( key );
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();
      
      return null;
    }
  }

  /**
   * Checks if this one is more recent than the one in the cache and eventually stores it in the cache
   */
  public void check( final WQTableSet wqTableSet, final String tsInfoName, final Date to )
  {
    final StringValidityKey key = new StringValidityKey( tsInfoName, to );

    checkValidity( key );

    final StringValidityKey cacheKey = m_cache.getRealKey( key );

    if( cacheKey != null && cacheKey.getValidity().after( to ) )
      return; // no need to overwrite if more recent is in the cache

    m_cache.addObject( key, wqTableSet );
  }
  
  /**
   * Removes entries that are too old from the cache.
   */
  private void checkValidity( final StringValidityKey key )
  {
    final StringValidityKey realKey = m_cache.getRealKey( key );
    if( realKey == null )
      return;

    final Date validity = realKey.getValidity();

    final long cacheTime = validity.getTime();
    final long currentTime = System.currentTimeMillis();

    // If the object is too old, remove it from the cache
    if( currentTime - cacheTime > m_expireTime )
      m_cache.remove( key );
  }  
}
