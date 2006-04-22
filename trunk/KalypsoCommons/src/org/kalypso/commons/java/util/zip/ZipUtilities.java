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
package org.kalypso.commons.java.util.zip;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import org.apache.commons.io.CopyUtils;
import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.io.FileUtilities;

/**
 * @author belger
 */
public class ZipUtilities
{
  private ZipUtilities()
  {
  // wird nicht instantiiert
  }

  public static void unzip( final File zip, final File targetdir ) throws ZipException, IOException
  {
    InputStream zipIS = null;
    try
    {

      zipIS = new BufferedInputStream( new FileInputStream( zip ) );
      unzip( zipIS, targetdir );
    }
    finally
    {
      IOUtils.closeQuietly( zipIS );
    }
  }

  public static void unzip( final InputStream inputStream, final File targetdir ) throws IOException
  {
    final ZipInputStream zis = new ZipInputStream( inputStream );
    while( true )
    {
      final ZipEntry entry = zis.getNextEntry();
      if( entry == null )
        break;

      final File newfile = new File( targetdir, entry.getName() );
      if( entry.isDirectory() )
        newfile.mkdirs();
      else
      {
        if( !newfile.getParentFile().exists() )
          newfile.getParentFile().mkdirs();

        OutputStream os = null;
        try
        {
          os = new BufferedOutputStream( new FileOutputStream( newfile ) );
          CopyUtils.copy( zis, os );
        }
        finally
        {
          IOUtils.closeQuietly( os );
          zis.closeEntry();
        }
      }
    }
  }

  /**
   * Puts given files into a zip archive.
   * 
   * @param zipfile
   *          Target file. will be created rep. overwritten.
   * @param files
   *          The files to zip
   * @param basedir
   *          If given (i.e. != null) zipentries are genereates as relativ to this basedir (alle files must be within
   *          this dir). If null, alle ZipEntries are create with full path.
   * @throws IOException
   *  
   */
  public static void zip( final File zipfile, final File[] files, final File basedir ) throws IOException
  {
    ZipOutputStream zos = null;
    try
    {
      zos = new ZipOutputStream( new BufferedOutputStream( new FileOutputStream( zipfile ) ) );

      for( int i = 0; i < files.length; i++ )
      {
        final File file = files[i];

        final String relativePathTo = FileUtilities.getRelativePathTo( basedir, file );
        writeZipEntry( zos, file, relativePathTo );
      }
    }
    finally
    {
      IOUtils.closeQuietly( zos );
    }
  }

  /**
   * 
   * @param zipfile
   *          file to write
   * @param dir
   *          dir to archive
   * @throws IOException
   */
  public static void zip( final File zipfile, final File dir ) throws IOException
  {
    final ZipOutputStream zos = new ZipOutputStream( new BufferedOutputStream( new FileOutputStream( zipfile ) ) );
    zip( zos, dir );
  }
  
  /**
   * Zip a dir into a zip-stream. the streamgets closed by this operation.
   */
  public static void zip( final ZipOutputStream zos, final File dir ) throws IOException
  {
    ZipFileVisitor visitor = new ZipFileVisitor( zos );
    try
    {
      visitor.setBasePattern( dir.getAbsolutePath() );
      visitor.setBaseReplace( "" );
      FileUtilities.accept( dir, visitor, true );
    }
    finally
    {
      visitor.close();
    }
  }

  /**
   * Writes a single File into a Zip-Stream
   * 
   * @param pathname
   *          The name of the zip entry (relative Path into zip archive).
   */
  public static void writeZipEntry( final ZipOutputStream zos, final File file, final String pathname )
      throws IOException
  {
    final ZipEntry newEntry = new ZipEntry( pathname );
    zos.putNextEntry( newEntry );

    InputStream contentStream = null;

    try
    {
      contentStream = new BufferedInputStream( new FileInputStream( file ) );

      CopyUtils.copy( contentStream, zos );
    }
    finally
    {
      IOUtils.closeQuietly( contentStream );
    }

    zos.closeEntry();
  }
}