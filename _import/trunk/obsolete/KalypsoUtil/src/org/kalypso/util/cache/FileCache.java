package org.kalypso.util.cache;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
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
import org.kalypso.util.serializer.ISerializer;

/**
 * FileCache is an object cache using files and a custom serializing mechnismus.
 * The convention is to always use IFileCacheKey as keys. Failing to do so will
 * result in IllegalArgumentException. In order for objects to be serialized,
 * the ISerializer constructor argument needs to be specified.
 * 
 * @author schlienger
 */
public class FileCache
{
  private final static String INDEX_FILE = ".filecache";

  private final Comparator m_kc;

  private final ISerializer m_ser;

  private final File m_directory;

  /** maps the keys to files */
  private final Map m_index;

  private final IKeyFactory m_keyFactory;

  /**
   * Constructor
   * 
   * @param kFact
   *          the key factory used to read and write the keys from/to a simple
   *          string representation
   * @param kc
   *          key comparator
   * @param ser
   *          object serializer
   * @param directory
   *          location of the index file and of all other files used for caching
   */
  public FileCache( final IKeyFactory kFact, final Comparator kc,
      final ISerializer ser, final File directory )
  {
    if( !directory.exists() || !directory.isDirectory() )
      throw new IllegalArgumentException( "Argument is not a directory: "
          + directory.toString() );

    m_keyFactory = kFact;
    m_kc = kc;
    m_ser = ser;
    m_directory = directory;

    m_index = new TreeMap( m_kc );

    readIndexFile();
  }

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
      reader = new BufferedReader( new InputStreamReader( new FileInputStream(
          indexFile ) ) );

      String line = reader.readLine();
      while( line != null )
      {
        if( line.length() > 0 )
        {
          final String[] items = line.split( ";" );
          final String keySpec = items[0];
          final String fileName = items[1];

          final Object key = m_keyFactory.createKey( keySpec );
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
      writer = new BufferedWriter( new OutputStreamWriter(
          new FileOutputStream( indexFile ) ) );

      for( final Iterator it = m_index.entrySet().iterator(); it.hasNext(); )
      {
        final Map.Entry entry = (Entry) it.next();
        final String keySpec = m_keyFactory.toString( entry.getKey() );
        final String fileName = ((File) entry.getValue()).getName();

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

  public void addObject( final Object key, final Object object )
  {
    final File file;
    OutputStream os = null;

    try
    {
      if( m_index.containsKey( key ) )
        file = (File) m_index.get( key );
      else
        file = File.createTempFile( "cache", ".item", m_directory );

      os = new FileOutputStream( file );

      m_ser.write( object, os );

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

  public Object getObject( final Object key )
  {
    final File file = (File) m_index.get( key );
    if( file == null )
      return null;

    InputStream ins = null;
    try
    {
      ins = new FileInputStream( file );

      return m_ser.read( ins );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

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

  public void remove( final Object key )
  {
    if( m_index.containsKey( key ) )
    {
      final File file = (File) m_index.get( key );
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
   * TRICKY: Returns the real key for the given key. Since we are using
   * comparators for the internal index, it is possible that the real instance
   * of the key is some other object than the key used for querying the cache.
   * <p>
   * This is a convenient method that might be used by clients wishing to
   * perform additional key-comparisons.
   * 
   * @param key
   * @return
   */
  public Object getRealKey( final Object key )
  {
    if( m_index.containsKey( key ) )
    {
      final Object[] keys = m_index.keySet().toArray();
      final int ix = Arrays.binarySearch( keys, key, m_kc );
      return keys[ix];
    }

    return null;
  }
}
