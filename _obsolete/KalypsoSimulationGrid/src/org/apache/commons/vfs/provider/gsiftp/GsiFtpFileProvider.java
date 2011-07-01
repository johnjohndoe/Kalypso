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
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.commons.vfs.Capability;
import org.apache.commons.vfs.FileName;
import org.apache.commons.vfs.FileSystem;
import org.apache.commons.vfs.FileSystemConfigBuilder;
import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.FileSystemOptions;
import org.apache.commons.vfs.provider.AbstractOriginatingFileProvider;
import org.apache.commons.vfs.provider.GenericFileName;
import org.globus.ftp.GridFTPClient;
import org.globus.ftp.exception.ServerException;

/**
 * A provider for accessing files over GsiFTP.
 * 
 * @author <a href="mailto:vladimir_silva@yahoo.com">Vladimir Silva</a>
 * @version $Id: GsiFtpFileProvider.java 330337 2005-11-02 19:59:06Z imario $
 */
public class GsiFtpFileProvider extends AbstractOriginatingFileProvider
{
  private Log log = LogFactory.getLog( GsiFtpFileProvider.class );

  protected final static Collection<Capability> capabilities = Collections.unmodifiableCollection( Arrays.asList( new Capability[] { Capability.CREATE, Capability.DELETE, Capability.RENAME,
      Capability.GET_TYPE, Capability.LIST_CHILDREN, Capability.READ_CONTENT, Capability.URI, Capability.WRITE_CONTENT, Capability.GET_LAST_MODIFIED, Capability.SET_LAST_MODIFIED_FILE
  // Capability.RANDOM_ACCESS_READ
  } ) );

  public final static String ATTR_HOME_DIR = "HOME_DIRECTORY";

  public GsiFtpFileProvider( )
  {
    super();
    setFileNameParser( GsiFtpFileNameParser.getInstance() );
  }

  /**
   * @see org.apache.commons.vfs.provider.AbstractOriginatingFileProvider#doCreateFileSystem(org.apache.commons.vfs.FileName,
   *      org.apache.commons.vfs.FileSystemOptions)
   */
  @Override
  protected FileSystem doCreateFileSystem( final FileName name, final FileSystemOptions fileSystemOptions ) throws FileSystemException
  {
    // Create the file system
    final GenericFileName rootName = (GenericFileName) name;

    // Session session;
    GridFTPClient client;
    String attr_home;
    log.debug( "Creating connection to GsiFTP Host:" + rootName.getHostName() + " Port:" + rootName.getPort() + " User:" + rootName.getUserName() + " Path:" + rootName.getPath() );

    client = GsiFtpClientFactory.createConnection( rootName.getHostName(), rootName.getPort(), fileSystemOptions );

    // go to the home for the user
    try
    {
      client.changeDir( "~" );
      attr_home = client.getCurrentDir();
      log.debug( "Current directory: " + attr_home );
    }
    catch( final ServerException e )
    {
      throw new FileSystemException( e );
    }
    catch( final IOException e )
    {
      throw new FileSystemException( e );
    }

    // set HOME dir attribute
    final GsiFtpFileSystem fs = new GsiFtpFileSystem( rootName, client, fileSystemOptions );
    fs.setAttribute( ATTR_HOME_DIR, attr_home );

    return fs;
  }

  /**
   * @see org.apache.commons.vfs.provider.AbstractFileProvider#getConfigBuilder()
   */
  @Override
  public FileSystemConfigBuilder getConfigBuilder( )
  {
    return GsiFtpFileSystemConfigBuilder.getInstance();
  }

  /**
   * @see org.apache.commons.vfs.provider.FileProvider#getCapabilities()
   */
  public Collection<Capability> getCapabilities( )
  {
    return capabilities;
  }
}