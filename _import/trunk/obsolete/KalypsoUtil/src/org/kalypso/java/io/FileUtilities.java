/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
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
import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
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
   * @param charMode
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
   * @param charMode
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
        final File existingFile = fileExistsInDir( prefix, suffix, System
            .getProperty( "java.io.tmpdir" ) );
        return existingFile;
      }
      catch( final FileNotFoundException ignored )
      {
        // ignored
        //ignored.printStackTrace();
      }
    }

    File tmp = File.createTempFile( prefix, suffix );
    tmp.deleteOnExit();

    makeFileFromStream( charMode, tmp, ins );

    return tmp;

  }

  /**
   * Wie
   * {@link #makeFileFromStream(boolean, String, String, InputStream, boolean)},
   * benutzt aber eine vorgegebene Dateiposition
   * 
   * @param charMode
   * @param file
   * @param ins
   * @throws IOException
   */
  public static void makeFileFromStream( final boolean charMode, final File file,
      final InputStream ins ) throws IOException
  {
    if( charMode )
    {
      final BufferedReader br = new BufferedReader( new InputStreamReader( ins ) );
      final PrintWriter pw = new PrintWriter( new FileOutputStream( file ) );

      ReaderUtilities.readerCopy( br, pw );
    }
    else
    {
      final BufferedInputStream in = new BufferedInputStream( ins );
      final BufferedOutputStream out = new BufferedOutputStream( new FileOutputStream( file ) );

      StreamUtilities.streamCopy( in, out );
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
   * Rekursives l?schen von Dateien und Verzeichnissen
   * 
   * @param file
   *          Falls das Argument eine Datei ist, wird diese gel?scht. Ist es ein
   *          Verzeichnis, werden alle dieses mitsamt aller darin liegenden
   *          Verzeichnisse und Dateien gel?scht.
   */
  public static void deleteRecursive( final File file )
  {
    if( file == null )
      return;

    if( file.isDirectory() )
    {
      final File[] files = file.listFiles();
      for( int i = 0; i < files.length; i++ )
        deleteRecursive( files[i] );
    }

    file.delete();
  }

  /**
   * Creates a temp directory in java.io.tmpdir.
   * 
   * @param prefix
   * @return temporary directory
   * 
   * @see FileUtilities#createNewTempDir( String, File )
   */
  public static File createNewTempDir( final String prefix )
  {
    return createNewTempDir( prefix, new File( System.getProperty( "java.io.tmpdir" ) ) );
  }

  /**
   * Creates a temp directory inside the given one. It uses
   * <code>System.currentTimeMillis</code> for naming the new temp dir. This
   * method can hang a little while in the case the directory it tries to create
   * already exist.
   * 
   * @param prefix
   * @param parentDir
   * @return temporary directory
   */
  public static File createNewTempDir( final String prefix, final File parentDir )
  {
    while( true )
    {
      final File newDir = new File( parentDir, prefix + System.currentTimeMillis() );
      if( newDir.mkdir() )
        return newDir;
    }
  }

  /**
   * Macht aus einer absoluten Dateiangabe eine relative
   * 
   * @param basedir
   * @param absoluteFile
   * 
   * @return Ein File-Object, welches einen relativen Pfad enth?lt; null, wenn
   *         <code>basedir</code> kein Parent-Dir von
   *         <code>absoluteFile</code> ist
   */
  public static File getRelativeFileTo( final File basedir, final File absoluteFile )
  {
    final String rel = getRelativePathTo( basedir, absoluteFile );

    final File file = new File( "." + rel );
    return file;
  }

  /**
   * Returns the relative path, without any reserved characters such as '.'.
   * This is meant to be used without string concatenation function to reproduce
   * an absolute path again. Directly creating a File object on the path
   * returned by this method won't produce a good result. Use the
   * <code>getRelativeFileTo()</code> method instead.
   * 
   * @param basedir
   * @param absoluteFile
   * @return the relative path from absoluteFile to basedir
   */
  public static String getRelativePathTo( final File basedir, final File absoluteFile )
  {
    final String baseAbs = basedir.getAbsolutePath();
    final String absAbs = absoluteFile.getAbsolutePath();
    if( !absAbs.startsWith( baseAbs ) )
    {
      String difference = StringUtils.difference( baseAbs, absAbs );
      if( difference == null || "".equals( difference ) )
        return null;
      final int index = absAbs.indexOf( difference );
      if( index < 5 )
        return null;
      String back = baseAbs.substring( index );
      // TODO change regExp to "everything except fileseparator"
      String x = back.replaceAll( "[a-zA-Z0-9]+", ".." );
      String result = x + File.separator + difference;
      return result;
    }
    final String rel = absAbs.length() == baseAbs.length() ? "" : absAbs.substring( baseAbs
        .length() );

    return rel;
  }

  /**
   * Returns true if childCandidate is stored under the path of parent, either
   * directly or in a sub directory.
   * 
   * @param parent
   * @param childCandidate
   * @return true if childCandidate is a child of the given parent.
   */
  public static boolean isChildOf( final File parent, final File childCandidate )
  {
    File f = childCandidate;

    while( f != null )
    {
      if( f.equals( parent ) )
        return true;

      f = f.getParentFile();
    }

    return false;
  }

  /**
   * @param name
   *          name of path of the file
   * @return characters after last "." of given file name
   */
  public static String getSuffix( final String name )
  {
    final String[] strings = name.split( "\\." );
    if( strings.length != 0 )
      return strings[strings.length - 1];
    return null;
  }

  /**
   * @param file
   * @return characters after last "." of given file name
   */
  public static String getSuffix( final File file )
  {
    return getSuffix( file.getAbsolutePath() );
  }

  /**
   * Returns only the name part of the given file name removing the extension
   * part.
   * <p>
   * Example:
   * 
   * <pre>
   * 
   *  test.foo -- test
   *  robert.tt -- robert
   *  
   * </pre>
   * 
   * @param fileName
   * @return fileName without the last '.???' extension part (NOTE: the
   *         extension part is not limited to 3 chars)
   */
  public static String nameWithoutExtension( final String fileName )
  {
    final int lastIndexOf = fileName.lastIndexOf( '.' );
    if( lastIndexOf == -1 )
      return fileName;

    return fileName.substring( 0, lastIndexOf );
  }

  /**
   * Lässt den FileVisitor die angegebene Datei bzw. Verzeichnis und alle darin
   * enthaltenen Dateien besuchen.
   * 
   * @param recurse Falls true, werden auch Unterverzeichnisse besucht
   */
  public static void accept( final File root, final FileVisitor visitor, final boolean recurse )
  {
    // zuerst die Datei selbst
    final boolean stop = !visitor.visit( root );
    if( stop || !root.isDirectory() )
      return;

    final File[] files = root.listFiles();
    if( files == null )
      return;

    for( int i = 0; i < files.length; i++ )
    {
      final File file = files[i];
      
      if( file.isFile() || ( file.isDirectory() && recurse  ) )
        accept( file, visitor, recurse );
    }
  }

  public static void copyShapeFileToDirectory( String shapeBase, File target )
  {
    File _shp;
    File _dbf;
    File _shx;
    File _sbn;
    File _sbx;
    if( target.isDirectory() )
    {
      try
      {
        _shp = new File( shapeBase + ".shp" );
        if( _shp.exists() )
          FileUtils.copyFileToDirectory( _shp, target );
        else
          return;
        _dbf = new File( shapeBase + ".dbf" );
        if( _dbf.exists() )
          FileUtils.copyFileToDirectory( _dbf, target );
        else
          return;
        _shx = new File( shapeBase + ".shx" );
        if( _shx.exists() )
          FileUtils.copyFileToDirectory( _shx, target );
        else 
          return;
        _sbn = new File( shapeBase + ".sbn" );
        if( _sbn.exists() )
          FileUtils.copyFileToDirectory( _sbn, target );
        _sbx = new File( shapeBase + ".sbx" );
        if( _sbn.exists() )
          FileUtils.copyFileToDirectory( _sbx, target );

      }
      catch( MalformedURLException e )
      {
        e.printStackTrace();
      }
      catch( IOException e )
      {
        e.printStackTrace();
      }
    }
  }
}