package org.kalypso.commons.cache;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.serializer.ISerializer;
import org.kalypso.contribs.java.io.StreamUtilities;

/**
 * FileCache is an object cache using files and a custom serializing mechnismus. The convention is to always use
 * IFileCacheKey as keys. Failing to do so will result in IllegalArgumentException. In order for objects to be
 * serialized, the ISerializer constructor argument needs to be specified.
 * 
 * @author schlienger
 */
public class FileCache<K, V>
{
  private final static String INDEX_FILE = ".filecache";

  private final Comparator< ? super K> m_kc;

  private final ISerializer<V> m_ser;

  private final File m_directory;

  /** maps the keys to files */
  private final Map<K, File> m_index;

  private final IKeyFactory<K> m_keyFactory;

  /**
   * Constructor
   * 
   * @param kFact
   *          the key factory used to read and write the keys from/to a simple string representation
   * @param kc
   *          key comparator
   * @param ser
   *          object serializer
   * @param directory
   *          location of the index file and of all other files used for caching
   */
  public FileCache( final IKeyFactory<K> kFact, final Comparator< ? super K> kc, final ISerializer<V> ser, final File directory )
  {
    if( !directory.exists() || !directory.isDirectory() )
      throw new IllegalArgumentException( "Argument is not a directory: " + directory.toString() );

    m_keyFactory = kFact;
    m_kc = kc;
    m_ser = ser;
    m_directory = directory;

    m_index = new TreeMap<K, File>( m_kc );

    readIndexFile();
  }

  @Override
  protected void finalize( ) throws Throwable
  {
    m_index.clear();

    super.finalize();
  }

  private final void readIndexFile( )
  {
    final File indexFile = new File( m_directory, INDEX_FILE );
    if( !indexFile.exists() )
      return;

    BufferedReader reader = null;
    try
    {
      reader = new BufferedReader( new InputStreamReader( new FileInputStream( indexFile ) ) );

      String line = reader.readLine();
      while( line != null )
      {
        if( line.length() > 0 )
        {
          final String[] items = line.split( ";" );
          final String keySpec = items[0];
          final String fileName = items[1];

          final K key = m_keyFactory.createKey( keySpec );
          final File file = new File( m_directory, fileName );

          m_index.put( key, file );
        }

        line = reader.readLine();
      }
    }
    catch( final Exception e ) // FileNotFoundException, IOException
    {
      e.printStackTrace();
      throw new IllegalStateException( e.getLocalizedMessage() );
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  private void writeIndexFile( )
  {
    final File indexFile = new File( m_directory, INDEX_FILE );

    BufferedWriter writer = null;
    try
    {
      writer = new BufferedWriter( new OutputStreamWriter( new FileOutputStream( indexFile ) ) );

      for( final Iterator<Entry<K, File>> it = m_index.entrySet().iterator(); it.hasNext(); )
      {
        final Map.Entry<K, File> entry = it.next();
        final String keySpec = m_keyFactory.toString( entry.getKey() );
        final String fileName = entry.getValue().getName();

        writer.write( keySpec );
        writer.write( ";" );
        writer.write( fileName );
        writer.newLine();
      }
    }
    catch( final Exception e ) // FileNotFoundException, IOException
    {
      e.printStackTrace();
      throw new IllegalStateException( e.getLocalizedMessage() );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  public void addObject( final K key, final V value )
  {
    final File file;
    OutputStream os = null;

    try
    {
      if( m_index.containsKey( key ) )
        file = m_index.get( key );
      else
        file = File.createTempFile( "cache", ".item", m_directory );

      os = new BufferedOutputStream( new FileOutputStream( file ) );

      m_ser.write( value, os );

      m_index.put( key, file );

      writeIndexFile();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( os );
    }
  }

  /**
   * Directly adds a file to this file cache. The must must be readable by the serializer. The file is copied into the
   * cache.
   */
  public void addFile( final K key, final File fileToAdd ) throws IOException
  {
    OutputStream os = null;
    InputStream is = null;

    try
    {
      final File file;
      if( m_index.containsKey( key ) )
        file = m_index.get( key );
      else
        file = File.createTempFile( "cache", ".item", m_directory );

      os = new BufferedOutputStream( new FileOutputStream( file ) );
      is = new BufferedInputStream( new FileInputStream( fileToAdd ) );

      StreamUtilities.streamCopy( is, os );

      m_index.put( key, file );

      writeIndexFile();
    }
    finally
    {
      IOUtils.closeQuietly( os );
      IOUtils.closeQuietly( is );
    }
  }

  public V getObject( final K key )
  {
    final File file = m_index.get( key );
    if( file == null )
      return null;

    InputStream ins = null;
    try
    {
      ins = new BufferedInputStream( new FileInputStream( file ) );

      return m_ser.read( ins );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      
      // if something goes wrong
      // delete this entry from the cache
      remove( key );

      return null;
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }

  public int size( )
  {
    return m_index.size();
  }

  public void remove( final K key )
  {
    if( m_index.containsKey( key ) )
    {
      final File file = m_index.get( key );
      file.delete();

      m_index.remove( key );

      writeIndexFile();
    }
  }

  public void clear( )
  {
    for( final Iterator it = m_index.values().iterator(); it.hasNext(); )
    {
      final File file = (File) it.next();
      file.delete();
    }

    final File indexFile = new File( m_directory, INDEX_FILE );
    indexFile.delete();

    m_index.clear();
  }

  /**
   * TRICKY: Returns the real key for the given key. Since we are using comparators for the internal index, it is
   * possible that the real instance of the key is some other object than the key used for querying the cache.
   * <p>
   * This is a convenient method that might be used by clients wishing to perform additional key-comparisons.
   * 
   * @param key
   * @return null if not found
   */
  @SuppressWarnings("unchecked")
  public K getRealKey( final K key )
  {
    if( m_index.containsKey( key ) )
    {
      final Class< ? > componentType;
      if( key.getClass().isArray() )
        componentType = key.getClass().getComponentType();
      else
        componentType = key.getClass();
      final K[] keys = m_index.keySet().toArray( (K[]) java.lang.reflect.Array.newInstance( componentType, m_index.size() ) );
      // final K[] keys = m_index.keySet().toArray( (K[])java.lang.reflect.Array
      // .newInstance(K, m_index.size()) );
      final int ix = Arrays.binarySearch( keys, key, m_kc );
      return keys[ix];
    }

    return null;
  }
}
