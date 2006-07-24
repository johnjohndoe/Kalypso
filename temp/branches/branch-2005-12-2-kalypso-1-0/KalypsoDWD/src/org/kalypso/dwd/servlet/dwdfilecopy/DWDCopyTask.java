/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.dwd.servlet.dwdfilecopy;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimerTask;

import org.apache.commons.io.IOUtils;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.FileType;
import org.apache.commons.vfs.impl.DefaultFileSystemManager;
import org.kalypso.dwd.DWDException;
import org.kalypso.dwd.DWDRasterHelper;

class DWDCopyTask extends TimerTask
{
  private final File m_destFile;

  private final SimpleDateFormat m_dateFormat;

  private final boolean m_srcDel;

  private final boolean m_destUpdate;

  private DefaultFileSystemManager m_fsManager;

  private String m_URI;

  private FileObject m_fo;

  private FileObject[] m_list;

  public DWDCopyTask( final String URI, final DefaultFileSystemManager fsManager, final String srcFormat, final boolean srcDel, final File destName, final boolean destUpdate )
  {
    m_srcDel = srcDel;
    m_dateFormat = new SimpleDateFormat( srcFormat );
    m_destFile = destName;
    m_destUpdate = destUpdate;

    this.m_URI = URI;
    this.m_fsManager = fsManager;
  }

  @Override
  public void run( )
  {
    FileObject newFile = null;

    try
    {
      /* Check for the file or the base file (this could be a directory). */
      m_fo = m_fsManager.resolveFile( m_URI );

      //DWDFileCopyServlet.LOG.info( "URI: " + m_fo.getName() );

      //DWDFileCopyServlet.LOG.info( "ROOT: " + m_fo.getFileSystem().getRoot() );

      if( m_fo.getType() != FileType.FOLDER )
      {
        System.out.println( "The URI " + m_URI + " is no folder." );
        return;
      }

      /* Get all elements in this directory. */
      m_list = m_fo.getChildren();

      if( m_list.length == 0 )
      {
        DWDFileCopyServlet.LOG.warning( "There are no files in the Source:" + m_fo.getName().toString() );
        return;
      }

      //DWDFileCopyServlet.LOG.info( "[StartOfFiles]" );

      ///* Traverse through all elements. */
      //for( int i = 0; i < m_list.length; i++ )
      //{
      //  DWDFileCopyServlet.LOG.info( m_list[i].getName().getBaseName().toString() );
      //}

      //DWDFileCopyServlet.LOG.info( "[EndOfFiles]" );

      /* Find the newest file. */
      newFile = getNewestFile( m_dateFormat );

      if( newFile == null )
        return;

      DWDFileCopyServlet.LOG.info( "Newest file: " + newFile.getName().getBaseName().toString());
    }
    catch( FileSystemException e )
    {
      DWDFileCopyServlet.LOG.warning( "Error resloving the URI: " + e.getLocalizedMessage() );
      return;
    }
    finally
    {
    }

    // looping twice over this code in the case an exception
    // occurs, we try it again...
    for( int i = 0; i < 2; i++ )
    {
      FileOutputStream os = null;
      InputStream is = null;

      try
      {
        // if dest file either does not exist or is not up to date, overwrite with current DWD forecast
        if( !m_destFile.exists() || m_destFile.lastModified() < newFile.getContent().getLastModifiedTime() )
        {
          /* Copy the newest file. */
          DWDFileCopyServlet.LOG.info( "Copying ..." );
          
          final File dwdDest;

          if( m_destUpdate )
            dwdDest = new File( m_destFile.getParentFile(), newFile.getName().getBaseName() );
          else
            dwdDest = m_destFile;

          DWDFileCopyServlet.LOG.info( "Copying DWD-File \"" + newFile.getName().getBaseName() + "\" to: " + dwdDest.getAbsolutePath() );

          os = new FileOutputStream( dwdDest );
          is = newFile.getContent().getInputStream();

          /* The copy operation. */
          IOUtils.copy( is, os );

          os.close();
          is.close();

          // update file contents
          if( m_destUpdate )
          {
            DWDFileCopyServlet.LOG.info( "Updating " + m_destFile.getName() + " from " + dwdDest );
            DWDRasterHelper.updateDWDFileContents( dwdDest, m_destFile, m_dateFormat );

            m_destFile.setLastModified( newFile.getContent().getLastModifiedTime() );

            final boolean deleted = dwdDest.delete();

            if( !deleted )
              DWDFileCopyServlet.LOG.warning( "Could not delete temp DWD-File \"" + dwdDest.getName() + "\"" );
          }
        }
        
        // delete source file if flag is set
        if( m_srcDel )
        {
          /* Delete the old files. */
          DWDFileCopyServlet.LOG.info( "Deleting " + newFile.getName().getBaseName() );

          final boolean deleted = newFile.delete();

          if( !deleted )
            DWDFileCopyServlet.LOG.warning( "Could not delete DWD-File \"" + newFile.getName().getBaseName() + "\"" );
        }

        // no exception, so end loop here
        return;
      }
      catch( final IOException e )
      {
        DWDFileCopyServlet.LOG.warning( "Could not copy DWD-File \"" + newFile.getName().getBaseName() + "\" to folder: " + m_destFile.getAbsolutePath() + " due to: " + e.getLocalizedMessage() );
      }
      catch( final DWDException e )
      {
        DWDFileCopyServlet.LOG.warning( "DWD-File could not be updated: " + e.getLocalizedMessage() );
      }
      finally
      {
        IOUtils.closeQuietly( is );
        IOUtils.closeQuietly( os );
      }

      try
      {
        // make some pause before continuing
        Thread.sleep( 500 );
      }
      catch( final InterruptedException ignored )
      {
        // empty
      }
    }
  }

  /**
   * Find the newest file, via the filename.
   */
  public FileObject getNewestFile( final SimpleDateFormat df ) throws FileSystemException
  {
    if( m_list == null )
      return null;

    FileObject result = null;
    Date date = null;

    /* Search newest... */
    for( int i = 0; i < m_list.length; i++ )
    {
      final FileObject file = m_list[i];

      if( m_list[i].getType() == FileType.FOLDER )
        continue;

      final Date testdate = getDateFromRaster( file, df );

      if( testdate == null )
        continue;

      if( result == null )
      {
        result = file;
        date = testdate;
      }
      else if( testdate.after( date ) )
      {
        result = file;
        date = testdate;
      }
    }

    return result;
  }

  /**
   * Return the date of the dwd forecast file. The date is coded in the file name. Example filename for dwd raster
   * format: "lm_2004_11_10_00" and its format would be 'lm_'yyyy'_'MM'_'dd'_'hh
   */
  public static Date getDateFromRaster( final FileObject file, final SimpleDateFormat df )
  {
    try
    {
      return df.parse( file.getName().getBaseName() );
    }
    catch( final ParseException e )
    {
      DWDFileCopyServlet.LOG.warning( "DWD-Forecast filename \"" + file.getName().getBaseName().toString() + "\" has not a valid format, should be:" + df.toPattern() );
      return null;
    }
  }
}