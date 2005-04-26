package org.kalypso.wiskiadapter;

import java.io.File;
import java.util.Comparator;
import java.util.Date;

import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableSet;
import org.kalypso.util.cache.FileCache;
import org.kalypso.util.cache.IKeyFactory;

/**
 * RatingTableCache
 * 
 * @author schlienger
 */
public class RatingTableCache
{
  private static RatingTableCache m_instance = null;

  private final FileCache m_cache;

  private RatingTableCache( )
  {
    m_cache = new FileCache( new KeyFactory(), new KeyComparator(),
        WQTableFactory.getInstance(), new File( WiskiUtils.getProperties()
            .getProperty( "CACHE_DIRECTORY" ) ) );
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
    return (WQTableSet) m_cache.getObject( new RatingTableKey( String
        .valueOf( wiskiId ), validity ) );
  }

  /**
   * Checks if this one is more recent than the one in the cache and eventually
   * stores it in the cache
   * 
   * @param wqTableSet
   * @param wiskiId
   * @param validity
   */
  public void check( final WQTableSet wqTableSet, final Long wiskiId,
      final Date validity )
  {
    final RatingTableKey key = new RatingTableKey( String.valueOf( wiskiId), validity );
    
    final RatingTableKey cacheKey = (RatingTableKey) m_cache.getRealKey( key );
    
    if( cacheKey != null && cacheKey.getValidity().after( key.getValidity() ) )
      return; // no need to overwrite if more recent in the cache
    
    m_cache.addObject( key, wqTableSet );
  }

  private static class RatingTableKey
  {
    private final String m_wiskiId;

    private final Date m_validity;

    public RatingTableKey( final String wiskiId, final Date validity )
    {
      m_wiskiId = wiskiId;
      m_validity = validity;
    }

    public Date getValidity( )
    {
      return m_validity;
    }

    public String getWiskiId( )
    {
      return m_wiskiId;
    }
  }

  private static class KeyComparator implements Comparator
  {
    public int compare( final Object o1, final Object o2 )
    {
      final RatingTableKey k1 = (RatingTableKey) o1;
      final RatingTableKey k2 = (RatingTableKey) o2;

      return k1.getWiskiId().compareTo( k2.getWiskiId() );
    }
  }

  private static class KeyFactory implements IKeyFactory
  {
    public Object createKey( final String string )
    {
      final String[] splits = string.split( "-v-" );
      final RatingTableKey key = new RatingTableKey( splits[0], new Date( Date
          .parse( splits[1] ) ) );

      return key;
    }

    public String toString( final Object key )
    {
      final RatingTableKey rtkey = (RatingTableKey) key;
      final StringBuffer sb = new StringBuffer();
      sb.append( rtkey.getWiskiId() ).append( "-v-" ).append(
          rtkey.getValidity() );

      return sb.toString();
    }
  }
}
