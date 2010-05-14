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
package org.apache.commons.vfs.provider.gsiftp;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collections;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.Vector;

import org.apache.commons.vfs.FileName;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.FileType;
import org.apache.commons.vfs.provider.AbstractFileObject;
import org.apache.commons.vfs.provider.GenericFileName;
import org.apache.commons.vfs.provider.UriParser;
import org.globus.ftp.FileInfo;
import org.globus.ftp.GridFTPClient;
import org.globus.ftp.MlsxEntry;
import org.globus.ftp.Session;
import org.globus.ftp.exception.ClientException;
import org.globus.ftp.exception.ServerException;
import org.globus.io.streams.GridFTPInputStream;
import org.globus.io.streams.GridFTPOutputStream;

/**
 * A GridFTP file.
 * 
 * @author <a href="mailto:adammurdoch@apache.org">Adam Murdoch</a>
 * @author Stefan Kurzbach
 */
public class GsiFtpFileObject extends AbstractFileObject
{

  private static final Map<String, FileInfo> EMPTY_FTP_FILE_MAP = Collections.unmodifiableMap( new TreeMap<String, FileInfo>() );

  private static final DateFormat DATE_FORMAT_UNIX = new SimpleDateFormat( "yyyyMMddHHmmss", Locale.US );

  static
  {
    // always use GMT for Unix times
    DATE_FORMAT_UNIX.setTimeZone( TimeZone.getTimeZone( "GMT" ) );
  }

  // Cached info
  private FileInfo m_fileInfo;

  private Map<String, FileInfo> m_children;

  /**
   *
   */
  protected GsiFtpFileObject( final FileName name, final GsiFtpFileSystem fileSystem )
  {
    super( name, fileSystem );
  }

  /**
   * Called by child file objects, to locate their ftp file info.
   * 
   * @param name
   *          the filename in its native form ie. without uri stuff (%nn)
   * @param flush
   *          recreate children cache
   */
  private FileInfo getChildFile( final String name, final boolean flush ) throws IOException
  {
    if( flush )
    {
      m_children = null;
    }

    // List the children of this file
    doGetChildren();

    // Look for the requested child
    FileInfo ftpFile = m_children.get( name );
    return ftpFile;
  }

  /**
   * Fetches the children of this file, if not already cached.
   */
  private void doGetChildren( ) throws IOException
  {
    final GridFTPClient client = getClient();
    try
    {
      client.setPassive();
      client.setLocalActive();
      client.setType( Session.TYPE_ASCII );
      final Vector<FileInfo> tmpChildren = getTheKiddies( client );

      if( tmpChildren == null || tmpChildren.size() == 0 )
      {
        m_children = EMPTY_FTP_FILE_MAP;
      }
      else
      {
        m_children = new TreeMap<String, FileInfo>();

        // Remove '.' and '..' elements
        for( int i = 0; i < tmpChildren.size(); i++ )
        {
          final FileInfo child = tmpChildren.get( i );

          if( child == null )
          {
            continue;
          }
          if( !".".equals( child.getName() ) && !"..".equals( child.getName() ) )
          {
            m_children.put( child.getName(), child );
          }
        }
      }
    }
    catch( ServerException se )
    {
      se.printStackTrace();
      throw new IOException( se.getMessage() );
    }
    catch( ClientException ce )
    {
      throw new IOException( ce.getMessage() );
    }
    finally
    {
      putClient( client );
    }
  }

  @SuppressWarnings("unchecked")
  private Vector<FileInfo> getTheKiddies( final GridFTPClient client ) throws ServerException, IOException, ClientException
  {
    final Vector<FileInfo> fileList = new Vector<FileInfo>();
    final Vector<MlsxEntry> mlsds = client.mlsd( getName().getPath() );
    for( final MlsxEntry mlsxEntry : mlsds )
    {
      final FileInfo fileInfo = new FileInfo();

      final String fileName = mlsxEntry.getFileName();
      if( fileName == null )
        continue;
      fileInfo.setName( fileName );

      final String lastModified = mlsxEntry.get( MlsxEntry.MODIFY );
      fileInfo.setDate( lastModified );
      fileInfo.setTime( lastModified );

      final String type = mlsxEntry.get( MlsxEntry.TYPE );
      if( MlsxEntry.TYPE_DIR.equals( type ) )
        fileInfo.setFileType( FileInfo.DIRECTORY_TYPE );
      else if( MlsxEntry.TYPE_FILE.equals( type ) )
        fileInfo.setFileType( FileInfo.FILE_TYPE );
      else if( MlsxEntry.TYPE_CDIR.equals( type ) )
        continue;
      else if( MlsxEntry.TYPE_PDIR.equals( type ) )
        continue;
      else if( "link".equals( type ) )
        fileInfo.setFileType( FileInfo.SOFTLINK_TYPE );
      else
        fileInfo.setFileType( FileInfo.UNKNOWN_TYPE );

      final String size = mlsxEntry.get( MlsxEntry.SIZE );
      fileInfo.setSize( Long.parseLong( size ) );

      fileList.addElement( fileInfo );
    }
    return fileList;
  }

  /**
   * @see org.apache.commons.vfs.provider.AbstractFileObject#doAttach()
   */
  @Override
  protected void doAttach( ) throws IOException
  {
    // Get the parent folder to find the info for this file
    getInfo( false );
  }

  /**
   * Fetches the info for this file.
   */
  public void getInfo( final boolean flush ) throws IOException
  {
    final GsiFtpFileObject parent = (GsiFtpFileObject) getParent();
    final FileInfo newFileInfo;

    if( parent != null )
    {
      newFileInfo = parent.getChildFile( UriParser.decode( getName().getBaseName() ), flush );
    }
    else
    {
      // Assume the root is a directory and exists
      newFileInfo = new FileInfo();

      // GridFTP only runs in UNIX so this would be OK
      newFileInfo.setName( "/" );
      newFileInfo.setFileType( FileInfo.DIRECTORY_TYPE );
    }

    this.m_fileInfo = newFileInfo;
  }

  /**
   * @see org.apache.commons.vfs.provider.AbstractFileObject#doDetach()
   */
  @Override
  protected void doDetach( )
  {
    this.m_fileInfo = null;
    m_children = null;
  }

  /**
   * @see org.apache.commons.vfs.provider.AbstractFileObject#onChildrenChanged(org.apache.commons.vfs.FileName,
   *      org.apache.commons.vfs.FileType)
   */
  @Override
  protected void onChildrenChanged( final FileName child, final FileType newType )
  {
    if( m_children != null && newType.equals( FileType.IMAGINARY ) )
    {
      try
      {
        m_children.remove( UriParser.decode( child.getBaseName() ) );
      }
      catch( final FileSystemException e )
      {
        throw new RuntimeException( e.getMessage() );
      }
    }
    else
    {
      // if child was added we have to rescan the children
      // TODO - get rid of this
      m_children = null;
    }
  }

  /**
   * @see org.apache.commons.vfs.provider.AbstractFileObject#onChange()
   */
  @Override
  protected void onChange( ) throws IOException
  {
    m_children = null;

    if( getType().equals( FileType.IMAGINARY ) )
    {
      // file is deleted, avoid server lookup
      this.m_fileInfo = null;
      return;
    }

    getInfo( true );
  }

  /**
   * @see org.apache.commons.vfs.provider.AbstractFileObject#doGetType()
   */
  @Override
  protected FileType doGetType( ) throws Exception
  {
    if( this.m_fileInfo == null )
    {
      return FileType.IMAGINARY;
    }
    else if( this.m_fileInfo.isDirectory() )
    {
      return FileType.FOLDER;
    }
    else if( this.m_fileInfo.isFile() )
    {
      return FileType.FILE;
    }
    else if( this.m_fileInfo.isSoftLink() )
    {
      // TODO: handle links
      return FileType.IMAGINARY;
    }

    throw new FileSystemException( "vfs.provider.gsiftp/get-type.error", getName() );
  }

  /**
   * @see org.apache.commons.vfs.provider.AbstractFileObject#doListChildren()
   */
  @Override
  protected String[] doListChildren( ) throws Exception
  {
    // List the children of this file
    doGetChildren();

    // TODO - get rid of this children stuff
    final String[] childNames = new String[m_children.size()];
    int childNum = -1;
    Iterator<FileInfo> iterChildren = m_children.values().iterator();

    while( iterChildren.hasNext() )
    {
      childNum++;
      final FileInfo child = iterChildren.next();
      childNames[childNum] = child.getName();
    }

    return UriParser.encode( childNames );
  }

  /**
   * @see org.apache.commons.vfs.provider.AbstractFileObject#doDelete()
   */
  @Override
  protected void doDelete( ) throws Exception
  {
    final GridFTPClient ftpClient = getClient();
    try
    {
      final String path = getName().getPath();
      if( this.m_fileInfo.isDirectory() )
      {
        ftpClient.deleteDir( path );
      }
      else
      {
        ftpClient.deleteFile( path );
      }
    }
    finally
    {
      putClient( ftpClient );
      this.m_fileInfo = null;
      m_children = EMPTY_FTP_FILE_MAP;
    }
  }

  /**
   * @see org.apache.commons.vfs.provider.AbstractFileObject#doRename(org.apache.commons.vfs.FileObject)
   */
  @Override
  protected void doRename( final FileObject newfile ) throws Exception
  {
    final GridFTPClient ftpClient = getClient();
    try
    {
      final String oldName = getName().getPath();
      final String newName = newfile.getName().getPath();
      ftpClient.rename( oldName, newName );
    }
    finally
    {
      putClient( ftpClient );
      this.m_fileInfo = null;
      m_children = EMPTY_FTP_FILE_MAP;
    }
  }

  /**
   * @see org.apache.commons.vfs.provider.AbstractFileObject#doCreateFolder()
   */
  @Override
  protected void doCreateFolder( ) throws Exception
  {
    final GridFTPClient client = getClient();
    try
    {
      final String path = getName().getPath();
      client.makeDir( path );
    }
    finally
    {
      putClient( client );
    }
  }

  /**
   * @see org.apache.commons.vfs.provider.AbstractFileObject#doGetContentSize()
   */
  @Override
  protected long doGetContentSize( ) throws Exception
  {
    return this.m_fileInfo.getSize();
  }

  /**
   * @see org.apache.commons.vfs.provider.AbstractFileObject#doGetLastModifiedTime()
   */
  @Override
  protected long doGetLastModifiedTime( ) throws Exception
  {
    final String timeString = m_fileInfo.getTime();
    try
    {
      final long time = DATE_FORMAT_UNIX.parse( timeString ).getTime();
      return time;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return 0L;
    }
  }

  /**
   * @see org.apache.commons.vfs.provider.AbstractFileObject#doGetInputStream()
   */
  @Override
  protected InputStream doGetInputStream( ) throws Exception
  {
    final GenericFileName rootName = (GenericFileName) getFileSystem().getRootName();
    final String host = rootName.getHostName();
    final int port = rootName.getPort();

    final GridFTPInputStream fis = new GridFTPInputStream( null, host, port, getName().getPath() );
    return fis;
  }

  /**
   * Sets the file permissions to 755 (executable)
   * 
   * @throws FileSystemException
   */
  public void makeExecutable( ) throws FileSystemException
  {
    final GridFTPClient client = getClient();
    try
    {
      client.site( "chmod 744 " + getName().getPath() );
    }
    catch( final ServerException e )
    {
      throw new FileSystemException( e );
    }
    catch( final IOException e )
    {
      throw new FileSystemException( e );
    }
    finally
    {
      putClient( client );
    }
  }

  private void putClient( final GridFTPClient client )
  {
    ((GsiFtpFileSystem) getFileSystem()).putClient( client );
  }

  private GridFTPClient getClient( )
  {
    return ((GsiFtpFileSystem) getFileSystem()).getClient();
  }

  /**
   * @see org.apache.commons.vfs.provider.AbstractFileObject#doGetOutputStream(boolean)
   */
  @Override
  protected OutputStream doGetOutputStream( boolean bAppend ) throws Exception
  {
    final GenericFileName rootName = (GenericFileName) getFileSystem().getRootName();
    final String host = rootName.getHostName();
    final int port = rootName.getPort();
    final FileName name = getName();
    final String fileName = name.getPath();
    return new GridFTPOutputStream( null, host, port, fileName, bAppend );
  }
}
