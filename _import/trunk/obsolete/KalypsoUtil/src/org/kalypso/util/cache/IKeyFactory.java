package org.kalypso.util.cache;

/**
 * IFileCacheKey is a key that can be used with the FileCache. A Key in the FileCache must obey to this contract because
 * of the following reasons:
 * <ul>
 * <li>a key must be coded as a string in order to store the list of keys as a simple text file
 * <li>the list of keys serve as a simple index which is built once the FileCache is created and each time an object is
 * added or replaced in the cache.
 * </ul>
 * 
 * @author schlienger
 */
public interface IKeyFactory
{
  /**
   * Creates a new key using the given string
   * 
   * @param string
   *          some string representation of the key
   */
  public Object createKey( final String string );

  /**
   * Return a string representation of the given key
   * 
   * @return string representation of the key
   */
  public String toString( final Object key );
}
