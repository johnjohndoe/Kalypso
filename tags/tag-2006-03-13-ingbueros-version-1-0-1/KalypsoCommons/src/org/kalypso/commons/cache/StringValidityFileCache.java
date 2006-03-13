package org.kalypso.commons.cache;

import java.io.File;

import org.kalypso.commons.serializer.ISerializer;

/**
 * StringValidityFileCache
 * 
 * @author schlienger
 */
public class StringValidityFileCache
{
  private final FileCache<StringValidityKey> m_cache;

  public StringValidityFileCache( final ISerializer objectSerializer, final File directory )
  {
    m_cache = new FileCache<StringValidityKey>( new StringValidityKeyFactory(), StringValidityKey.createComparatorForStringCompareOnly(),
        objectSerializer, directory );
  }

  /**
   * Tries to fetch it from the cache, if available
   * 
   * @return null if not found in cache
   */
  public Object get( final StringValidityKey key )
  {
    return m_cache.getObject( key );
  }

  public void addObject( final StringValidityKey key, final Object object )
  {
    m_cache.addObject( key, object );
  }

  public StringValidityKey getRealKey( final StringValidityKey key )
  {
    return (StringValidityKey)m_cache.getRealKey( key );
  }
}
