package org.kalypso.java.io;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.URL;
import java.util.Random;

import org.kalypso.java.io.filter.PrefixSuffixFilter;

/**
 * Utility class for io and files
 * 
 * @author schlienger
 */
public class FileUtilities
{
  /**
   * See makeFileFromStream(). this method calls makeFileFromStream with
   * url.openStream() as parameter.
   * 
   * @param prefix
   *          prefix of new file name
   * @param suffix
   *          suffix of new file name
   * @param url
   *          data is read from this url
   * @param useCache
   *          if true tries to use an existing file with these prefix/suffix
   * 
   * @return newly created file
   * 
   * @throws IOException
   *           there are problems!
   */
  public static File makeFileFromUrl( boolean charMode, final String prefix, final String suffix,
      URL url, boolean useCache ) throws IOException
  {
    return makeFileFromStream( charMode, prefix, suffix, url.openStream(), useCache );
  }

  /**
   * Creates a new temporary file given its pathName and an InputStream. The
   * content from the InputStream is written into the file. The file will be
   * deleted after the VM shuts down
   * 
   * @param prefix
   *          prefix of file name
   * @param suffix
   *          suffix of file name
   * @param ins
   *          the input stream, that is the source
   * @param useCache
   *          if true tries to use an existing file with these prefix/suffix
   * 
   * @return the newly created file or null if an exception was thrown.
   * 
   * @throws IOException
   *           problems reading from stream or writing to temp. file
   */
  public static File makeFileFromStream( boolean charMode, final String prefix,
      final String suffix, InputStream ins, boolean useCache ) throws IOException
  {
    if( useCache )
    {
      try
      {
        final File existingFile = fileExistsInDir( prefix, suffix, System.getProperty( "java.io.tmpdir" ) );
        return existingFile;
      }
      catch( final FileNotFoundException ignored )
      {
        // ignored
      }
    }

    File tmp = File.createTempFile( prefix, suffix );
    tmp.deleteOnExit();

    makeFileFromStream( charMode, tmp, ins );
    
    return tmp;

  }
  
  /**
   * Wie {@link #makeFileFromStream(boolean, String, String, InputStream, boolean)}, benutzt aber
   * eine vorgegebene Dateiposition 
   */
  public static void makeFileFromStream( final boolean charMode, final File file, final InputStream ins ) throws IOException
  {
    if( charMode )
    {
      final BufferedReader br = new BufferedReader( new InputStreamReader( ins ) );
      final PrintWriter pw = new PrintWriter( new FileOutputStream( file ) );
      
      ReaderUtilities.readerCopy(br, pw);
//      while( br.ready() )
//      {
//        final String str = br.readLine();
//
//        if( str == null )
//          break;
//
//        pw.println( str );
//      }
//
//      br.close();
//      pw.close();
    }
    else
    {
      final BufferedInputStream in = new BufferedInputStream( ins );
      final BufferedOutputStream out = new BufferedOutputStream( new FileOutputStream( file ) );

      StreamUtilities.streamCopy(in, out);
//      int b = in.read();
//
//      while( b != -1 )
//      {
//        out.write( b );
//
//        b = in.read();
//      }
//
//      in.close();
//      out.close();
    }

  }
  

  /**
   * Looks in the given path if a file with the given prefix and suffix exists.
   * Returns the file in the positive. If more than one such file is found,
   * returns the first of them.
   * 
   * @param prefix
   *          name of the file should begin with this prefix
   * @param suffix
   *          name of the file should end with this suffix
   * @param path
   * 
   * @return the (first) File found
   * 
   * @throws FileNotFoundException
   *           when file was not found or path does not denote a directory
   * 
   * @see PrefixSuffixFilter
   */
  public static File fileExistsInDir( String prefix, String suffix, String path )
      throws FileNotFoundException
  {
    File dir = new File( path );

    if( dir.isDirectory() )
    {
      PrefixSuffixFilter filter = new PrefixSuffixFilter( prefix, suffix );

      File[] files = dir.listFiles( filter );

      if( files.length > 0 )
        return files[0];
    }

    throw new FileNotFoundException( "File with prefix (" + prefix + ") and suffix (" + suffix
        + ") was not found in " + path );
  }

  /**
   * Rekursives l�schen von Dateien und Verzeichnissen
   * 
   * @param file
   *          Falls das Argument eine Datei ist, wird diese gel�scht. Ist es ein
   *          Verzeichnis, werden alle dieses mitsamt aller darin liegenden
   *          Verzeichnisse und Dateien gel�scht.
   */
  public static void deleteRecursive( final File file )
  {
    if( file.isDirectory() )
    {
      final File[] files = file.listFiles();
      for( int i = 0; i < files.length; i++ )
        deleteRecursive( files[i] );
    }

    file.delete();
  }

  public static File createRandomTmpDir( final String prefix )
  {
    final File tmpDir = new File( System.getProperty( "java.io.tmpdir" ) );
    File dataDir = null;

    int count = 0;
    while( dataDir == null && count < 100 )
    {
      final File tmpDataDir = new File( tmpDir, prefix + new Random().nextInt() );
      if( tmpDataDir.mkdir() )
        return tmpDataDir;

      count++;
    }

    return null;
  }
}