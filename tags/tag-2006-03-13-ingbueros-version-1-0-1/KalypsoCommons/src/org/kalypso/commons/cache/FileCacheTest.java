package org.kalypso.commons.cache;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.lang.reflect.InvocationTargetException;
import java.util.Comparator;

import junit.framework.TestCase;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.serializer.ISerializer;

/**
 * CacheTest
 * 
 * @author schlienger
 */
public class FileCacheTest extends TestCase
{
  public void testGetObject()
  {
    final StringKeyFactory fact = new StringKeyFactory();
    final Comparator<String> kc = new StringComparator();
    final ISerializer ser = new StringSerializer();
    final FileCache<String> cache = new FileCache<String>( fact, kc, ser, new File( System.getProperty( "java.io.tmpdir" ) ) );

    cache.addObject( "A", "A" );
    cache.addObject( "B", "B" );
    cache.addObject( "C", "C" );
    cache.addObject( "D", "D" );

    assertEquals( cache.getObject( "A" ), "A" );
    assertEquals( cache.getObject( "B" ), "B" );
    assertEquals( cache.getObject( "C" ), "C" );
    assertEquals( cache.getObject( "D" ), "D" );

    cache.remove( "C" );

    assertTrue( cache.getObject( "C" ) == null );

    cache.clear();

    assertTrue( cache.size() == 0 );
    assertTrue( cache.getObject( "B" ) == null );
  }

  private static class StringComparator implements Comparator<String>
  {
    public int compare( String s1, String s2 )
    {
      return s1.compareTo( s2 );
    }
  }

  private static class StringKeyFactory implements IKeyFactory<String>
  {
    public String createKey( final String string )
    {
      return string;
    }

    public String toString( final String key )
    {
      return key;
    }
  }

  private static class StringSerializer implements ISerializer
  {
    public Object read( InputStream ins ) throws InvocationTargetException
    {
      BufferedReader r = null;
      try
      {
        r = new BufferedReader( new InputStreamReader( ins ) );

        return r.readLine();
      }
      catch( Exception e )
      {
        e.printStackTrace();
        throw new InvocationTargetException( e );
      }
      finally
      {
        IOUtils.closeQuietly( r );
      }
    }

    public void write( Object object, OutputStream os ) throws InvocationTargetException
    {
      BufferedWriter w = null;
      try
      {
        w = new BufferedWriter( new OutputStreamWriter( os ) );
        w.write( object.toString() );
      }
      catch( Exception e )
      {
        e.printStackTrace();
        throw new InvocationTargetException( e );
      }
      finally
      {
        IOUtils.closeQuietly( w );
      }
    }
  }
}
