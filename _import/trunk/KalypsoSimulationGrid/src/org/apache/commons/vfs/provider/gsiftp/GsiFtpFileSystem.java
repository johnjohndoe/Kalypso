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

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Semaphore;

import org.apache.commons.vfs.FileName;
import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileSystem;
import org.apache.commons.vfs.FileSystemOptions;
import org.apache.commons.vfs.provider.AbstractFileSystem;
import org.apache.commons.vfs.provider.GenericFileName;
import org.globus.ftp.GridFTPClient;

/**
 * Represents the files on an SFTP server.
 * 
 * @author <a href="mailto:adammurdoch@apache.org">Adam Murdoch</a>
 * @version $Revision$ $Date: 2005-10-14 10:59:47 -0700 (Fri, 14 Oct 2005) $
 */
public class GsiFtpFileSystem extends AbstractFileSystem implements FileSystem
{
  // The client to use for every file system request
  private GridFTPClient m_client;

  // lock on the client
  private final Semaphore idleClientSync = new Semaphore( 1 );

  // File system attributes
  private Map<String, Object> attribs = new HashMap<String, Object>();

  /**
   * @param rootName
   * @param ftpClient
   * @param fileSystemOptions
   */
  protected GsiFtpFileSystem( final GenericFileName rootName, final GridFTPClient ftpClient, final FileSystemOptions fileSystemOptions )
  {
    super( rootName, null, fileSystemOptions );
    m_client = ftpClient;
  }

  /**
   * @see org.apache.commons.vfs.provider.AbstractFileSystem#doCloseCommunicationLink()
   */
  @Override
  protected void doCloseCommunicationLink( )
  {
    // Clean up the connection
    try
    {
      m_client.close();
    }
    catch( final Exception e )
    {
      // gobble
    }
  }

  /**
   * @see org.apache.commons.vfs.provider.AbstractFileSystem#addCapabilities(java.util.Collection)
   */
  @SuppressWarnings("unchecked")
  @Override
  protected void addCapabilities( final Collection caps )
  {
    caps.addAll( GsiFtpFileProvider.capabilities );
  }

  /**
   * Creates an FTP client to use.
   */
  public GridFTPClient getClient( )
  {
    try
    {
      idleClientSync.acquire();
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
    }
    return m_client;
  }

  /**
   * Returns an FTP client after use.
   */
  public void putClient( final GridFTPClient client )
  {
    if( m_client != client )
    {
      // TODO: what to do?
      System.out.println( "Bad client" );
      return;
    }
    idleClientSync.release();
  }

  /**
   * @see org.apache.commons.vfs.provider.AbstractFileSystem#createFile(org.apache.commons.vfs.FileName)
   */
  @Override
  protected FileObject createFile( final FileName name )
  {
    return new GsiFtpFileObject( name, this );
  }

  /**
   * @see org.apache.commons.vfs.provider.AbstractFileSystem#setAttribute(java.lang.String, java.lang.Object)
   */
  @Override
  public void setAttribute( final String attrName, final Object value )
  {
    attribs.put( attrName, value );
  }

  /**
   * @see org.apache.commons.vfs.provider.AbstractFileSystem#getAttribute(java.lang.String)
   */
  @Override
  public Object getAttribute( final String attrName )
  {
    return attribs.get( attrName );
  }

}
