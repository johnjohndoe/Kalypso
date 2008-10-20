/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.commons.io;

import java.io.IOException;
import java.io.OutputStream;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;
import org.apache.commons.vfs.AllFileSelector;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.FileSystemManager;
import org.apache.commons.vfs.FileSystemOptions;
import org.apache.commons.vfs.FileType;
import org.apache.commons.vfs.FileUtil;
import org.apache.commons.vfs.UserAuthenticator;
import org.apache.commons.vfs.VFS;
import org.apache.commons.vfs.auth.StaticUserAuthenticator;
import org.apache.commons.vfs.impl.DefaultFileSystemManager;
import org.apache.commons.vfs.provider.http.HttpFileSystemConfigBuilder;
import org.apache.commons.vfs.provider.webdav.WebdavFileSystemConfigBuilder;
import org.kalypso.commons.Debug;
import org.kalypso.commons.net.ProxyUtilities;
import org.kalypso.contribs.eclipse.core.net.Proxy;

/**
 * Helpfull functions when dealing with VFS.
 * 
 * @author Holger Albert
 */
public class VFSUtilities
{
  private static final FileSystemOptions THE_WEBDAV_OPTIONS = new FileSystemOptions();

  private static final FileSystemOptions THE_HTTP_OPTIONS = new FileSystemOptions();

  private static final FileSystemOptions THE_HTTPS_OPTIONS = new FileSystemOptions();

  /**
   * The constructor.
   */
  private VFSUtilities( )
  {
  }

  /**
   * This function returns a single FileSystemManager with support for webdav.
   * 
   * @return The FileSystemManager.
   */
  public static FileSystemManager getManager( ) throws FileSystemException
  {
    DefaultFileSystemManager fsManager = (DefaultFileSystemManager) VFS.getManager();

    String[] schemes = fsManager.getSchemes();
    //
    // boolean found = false;
    // for( int i = 0; i < schemes.length; i++ )
    // {
    // if( schemes[i].equals( "webdav" ) )
    // found = true;
    // }
    //
    // if( found == false )
    // {
    // Debug.println( "Adding webdav file provider ..." );
    // fsManager.addProvider( "webdav", new WebdavFileProvider() );
    // }

    return fsManager;
  }

  /**
   * This function copies a source to a given destination. If no filename is given in the destination file handle, the
   * filename of the source is used.<br>
   * 
   * @param source
   *          The source file.
   * @param destination
   *          The destination file or path.
   */
  public static void copy( final FileObject source, final FileObject destination ) throws IOException
  {
    if( FileType.FOLDER.equals( source.getType() ) )
    {
      copyDirectoryToDirectory( source, destination );
      return;
    }

    copyFileTo( source, destination );
  }

  /**
   * This function copies a source file to a given destination. If no filename is given in the destination file handle,
   * the filename of the source is used.<br>
   * <br>
   * It is tried to copy the file three times. If all three tries has failed, only then an IOException is thrown. <br>
   * All other exceptions are thrown normally.
   * 
   * @param source
   *          The source file.
   * @param destination
   *          The destination file or path.
   */
  public static void copyFileTo( FileObject source, FileObject destination ) throws IOException
  {
    /* Some variables for handling the errors. */
    boolean success = false;
    int cnt = 0;

    while( success == false )
    {
      try
      {
        if( FileType.FOLDER.equals( source.getType() ) )
          throw new IllegalArgumentException( "The source-file is a directory ..." );

        /* If the destination is only a directory, use the sources filename for the destination file. */
        FileObject destinationFile = destination;
        if( FileType.FOLDER.equals( destination.getType() ) )
          destinationFile = destination.resolveFile( source.getName().getBaseName() );

        /* Copy file. */
        Debug.println( "Copy file '" + source.getName() + " to '" + destinationFile.getName() + "' ..." );
        FileUtil.copyContent( source, destinationFile );

        /* End copying of this file, because it was a success. */
        success = true;
      }
      catch( final IOException e )
      {
        /* An error has occurred while copying the file. */
        Debug.println( "An error has occured with the message: " + e.getLocalizedMessage() );

        /* If a certain amount (here 2) of retries was reached before, re-throw the error. */
        if( cnt >= 2 )
        {
          Debug.println( "The second retry has failed, rethrowing the error ..." );
          throw e;
        }

        /* Retry the copying of the file. */
        cnt++;
        Debug.println( "Retry: " + String.valueOf( cnt ) );
        success = false;

        /* Wait for some milliseconds. */
        try
        {
          Thread.sleep( 1000 );
        }
        catch( InterruptedException e1 )
        {
          /*
           * Runs in the next loop then and if no error occurs then, it is ok. If an error occurs again, it is an
           * exception thrown on the last failed retry or it is slept again.
           */
        }
      }
    }
  }

  /**
   * This function will copy one directory to another one. If the destination base directory does not exist, it will be
   * created.
   * 
   * @param source
   *          The source directory.
   * @param destination
   *          The destination directory.
   */
  public static void copyDirectoryToDirectory( FileObject source, FileObject destination ) throws IOException
  {
    if( !FileType.FOLDER.equals( source.getType() ) )
      throw new IllegalArgumentException( "Source must be directory...: " + source.getURL() );

    if( destination.exists() )
    {
      if( !FileType.FOLDER.equals( destination.getType() ) )
        throw new IllegalArgumentException( "Destination must be a directory...: " + destination.getURL() );
    }
    else
    {
      Debug.println( "Creating directory " + destination.getName() + " ..." );
      destination.createFolder();
    }

    FileObject[] children = source.getChildren();
    for( int i = 0; i < children.length; i++ )
    {
      FileObject child = children[i];
      if( FileType.FILE.equals( child.getType() ) )
      {
        /* Need a destination file with the same name as the source file. */
        FileObject destinationFile = destination.resolveFile( child.getName().getBaseName() );

        /* Copy ... */
        copyFileTo( child, destinationFile );
      }
      else if( FileType.FOLDER.equals( child.getType() ) )
      {
        /* Need the same name for destination directory, as the source directory has. */
        FileObject destinationDir = destination.resolveFile( child.getName().getBaseName() );

        /* Copy ... */
        Debug.println( "Copy directory " + child.getName() + " to " + destinationDir.getName() + " ..." );
        copyDirectoryToDirectory( child, destinationDir );
      }
      else
        Debug.println( "Could not determine the file type ..." );
    }
  }

  /**
   * This function copies a string to a vfs file object.
   * 
   * @param value
   *          This string will be copied to the file.
   * @param destination
   *          The destination. It must be a file.
   */
  public static void copyStringToFileObject( String value, FileObject destination ) throws IOException
  {
    if( FileType.FOLDER.equals( destination.getType() ) )
      throw new IllegalArgumentException( "Destination is a folder." );

    /* Copy the string to this url. */
    OutputStream outputStream = null;
    StringReader stringReader = null;

    try
    {
      outputStream = destination.getContent().getOutputStream( false );
      stringReader = new StringReader( value );
      IOUtils.copy( stringReader, outputStream );
      outputStream.close();
      stringReader.close();
    }
    finally
    {
      IOUtils.closeQuietly( outputStream );
      IOUtils.closeQuietly( stringReader );
    }
  }

  /**
   * This function creates a temporary directory, which has a unique file name.
   * 
   * @param prefix
   *          This prefix will be used for the temporary directory.
   * @param parentDir
   *          The parent directory. In it the new directory will be created.
   * @return The new unique directory.
   */
  public static FileObject createTempDirectory( final String prefix, final FileObject parentDir ) throws FileSystemException
  {
    FileSystemManager fsManager = getManager();

    while( true )
    {
      String dirParent = parentDir.getURL().toExternalForm();
      String dirName = prefix + String.valueOf( System.currentTimeMillis() );

      FileObject newDir = fsManager.resolveFile( dirParent + "/" + dirName );
      if( newDir.exists() )
        continue;

      Debug.println( "Creating folder " + newDir.getName().getPath() + " ..." );
      newDir.createFolder();
      return newDir;
    }
  }

  /**
   * This function will check the string for protocol, and if neccessary applys an proxy object to it.
   * 
   * @param absoluteFile
   *          The absolute file path to the file. It should be absolute, because this function was not testet against
   *          relative files.
   * @return The file object.
   */
  public static FileObject checkProxyFor( String absoluteFile ) throws FileSystemException, MalformedURLException
  {
    FileSystemManager fsManager = getManager();

    Proxy proxy = ProxyUtilities.getProxy();
    Debug.println( "Should use proxy: " + String.valueOf( proxy.useProxy() ) );

    if( proxy.useProxy() && !ProxyUtilities.isNonProxyHost( new URL( absoluteFile ) ) )
    {
      String proxyHost = proxy.getProxyHost();
      int proxyPort = proxy.getProxyPort();
      Debug.println( "Proxy host: " + proxyHost );
      Debug.println( "Proxy port: " + String.valueOf( proxyPort ) );

      /* Get the credentials. */
      String user = proxy.getUser();
      String password = proxy.getPassword();

      Pattern p = Pattern.compile( "(.+)://.+" );
      Matcher m = p.matcher( absoluteFile );
      if( m.find() == true )
      {
        Debug.println( "File: " + absoluteFile );
        Debug.println( "Protocol: " + m.group( 1 ) );

        if( m.group( 1 ).equals( "webdav" ) )
        {
          WebdavFileSystemConfigBuilder.getInstance().setProxyHost( THE_WEBDAV_OPTIONS, proxyHost );
          WebdavFileSystemConfigBuilder.getInstance().setProxyPort( THE_WEBDAV_OPTIONS, proxyPort );

          /* If there are credentials given, set them. */
          if( user != null && password != null )
          {
            UserAuthenticator authenticator = new StaticUserAuthenticator( null, user, password );
            WebdavFileSystemConfigBuilder.getInstance().setProxyAuthenticator( THE_WEBDAV_OPTIONS, authenticator );
          }

          return fsManager.resolveFile( absoluteFile, THE_WEBDAV_OPTIONS );
        }
        else if( m.group( 1 ).equals( "http" ) )
        {
          HttpFileSystemConfigBuilder.getInstance().setProxyHost( THE_HTTP_OPTIONS, proxyHost );
          HttpFileSystemConfigBuilder.getInstance().setProxyPort( THE_HTTP_OPTIONS, proxyPort );

          /* If there are credentials given, set them. */
          if( user != null && password != null )
          {
            UserAuthenticator authenticator = new StaticUserAuthenticator( null, user, password );
            HttpFileSystemConfigBuilder.getInstance().setProxyAuthenticator( THE_HTTP_OPTIONS, authenticator );
          }

          return fsManager.resolveFile( absoluteFile, THE_HTTP_OPTIONS );
        }
        else if( m.group( 1 ).equals( "https" ) )
        {
          HttpFileSystemConfigBuilder.getInstance().setProxyHost( THE_HTTPS_OPTIONS, proxyHost );
          HttpFileSystemConfigBuilder.getInstance().setProxyPort( THE_HTTPS_OPTIONS, proxyPort );

          /* If there are credentials given, set them. */
          if( user != null && password != null )
          {
            UserAuthenticator authenticator = new StaticUserAuthenticator( null, user, password );
            HttpFileSystemConfigBuilder.getInstance().setProxyAuthenticator( THE_HTTPS_OPTIONS, authenticator );
          }

          return fsManager.resolveFile( absoluteFile, THE_HTTPS_OPTIONS );
        }
      }
    }

    return fsManager.resolveFile( absoluteFile );
  }

  /**
   * This function deletes the given file. If the file object is a directory, all content and the directory itself will
   * be deleted.
   * 
   * @param toDel
   *          The file or directory to be deleted.
   * @return The number of deleted files. 0, if none has been deleted.
   */
  public static int deleteFiles( FileObject toDel ) throws FileSystemException
  {
    if( FileType.FOLDER.equals( toDel.getType() ) )
    {
      /* Delete the directory. */
      Debug.println( "Deleting the directory " + toDel.getName() + " ..." );
      return toDel.delete( new AllFileSelector() );
    }
    else if( FileType.FILE.equals( toDel.getType() ) )
    {
      /* Delete the file. */
      Debug.println( "Deleting the file " + toDel.getName() + " ..." );
      if( toDel.delete() )
        return 1;

      Debug.println( "Could not delete " + toDel.getName() + "!" );
      return 0;
    }
    else
    {
      /* The type of the file could not be determined, or it is an imaginary one. */
      Debug.println( "Could not delete " + toDel.getName() + "!" );
      return 0;
    }
  }
}