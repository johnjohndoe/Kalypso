/*--------------- Kalypso-Header ------------------------------------------

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

--------------------------------------------------------------------------*/

package org.kalypso.contribs.java.io;

import java.io.File;

/**
 * @author schlienger
 */
public class FileUtilities
{
  /** THE system tmp dir "java.io.tmpdir" */
  public static final File TMP_DIR = new File( System.getProperty( "java.io.tmpdir" ) );

  /** regex defining which are the invalid characters for a file name */
  /** invalid characters for Windows filenames are: \ / : * ? " < > | */
  public final static String INVALID_CHARACTERS = "[\\\\/:\\*\\?\"<>| ]";

  private FileUtilities()
  {
    // not intended to be instanciated
  }

  /**
   * Returns only the name part of the given file name removing the extension part.
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
   * @return fileName without the last '.???' extension part (NOTE: the extension part is not limited to 3 chars)
   */
  public static String nameWithoutExtension( final String fileName )
  {
    final int lastIndexOf = fileName.lastIndexOf( '.' );
    if( lastIndexOf == -1 )
      return fileName;

    return fileName.substring( 0, lastIndexOf );
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
   * Replaces all invalid characters from the given fileName so that it is valid against the OS-rules for naming files.
   * 
   * @return a valid filename that can be used to create a new file, special (invalid) characters are removed and
   *         replaced by the given replacement-string
   */
  public static String validateName( final String fileName, final String replacement )
  {
    return fileName.replaceAll( INVALID_CHARACTERS, replacement );
  }

  /**
   * Gets the name part of a path-like-string.
   * <p>
   * That is, everything after the last '/' or '\'.
   * </p>
   * <p>
   * E.g. <code>C:/mydirectory/file.txt</code> gets <code>file.txt</code>
   * </p>.
   */
  public static String nameFromPath( final String path )
  {
    final int lastIndexOfSlash = path.lastIndexOf( '/' );
    final int lastIndexOfBackslash = path.lastIndexOf( '/' );
    final int lastIndexOf = Math.max( lastIndexOfSlash, lastIndexOfBackslash );

    if( lastIndexOf == -1 )
      return path;

    if( lastIndexOf + 1 == path.length() - 1 )
      return "";

    return path.substring( lastIndexOf + 1 );
  }
  
  /**
   * Rekursives l?schen von Dateien und Verzeichnissen
   * 
   * @param file
   *          Falls das Argument eine Datei ist, wird diese gel?scht. Ist es ein Verzeichnis, werden alle dieses mitsamt
   *          aller darin liegenden Verzeichnisse und Dateien gel?scht.
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
    return createNewTempDir( prefix, TMP_DIR );
  }
  
  /**
   * Creates a temp directory inside the given one. It uses <code>System.currentTimeMillis</code> for naming the new
   * temp dir. This method can hang a little while in the case the directory it tries to create already exist.
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
   * Returns true if childCandidate is stored under the path of parent, either directly or in a sub directory.
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
}
