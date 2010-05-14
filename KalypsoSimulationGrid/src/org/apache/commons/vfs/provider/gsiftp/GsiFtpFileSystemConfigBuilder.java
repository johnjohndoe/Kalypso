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

//import org.apache.commons.net.ftp.parser.FTPFileEntryParserFactory;

import org.apache.commons.vfs.FileSystemConfigBuilder;
import org.apache.commons.vfs.FileSystemOptions;
import org.ietf.jgss.GSSCredential;

//import java.io.File;

/**
 * The config builder for various sftp configuration options
 * 
 * @author <a href="mailto:imario@apache.org">Mario Ivankovits</a>
 * @version $Revision$ $Date: 2005-12-05 11:25:09 -0800 (Mon, 05 Dec 2005) $
 */
public class GsiFtpFileSystemConfigBuilder extends FileSystemConfigBuilder
{
  private final static GsiFtpFileSystemConfigBuilder builder = new GsiFtpFileSystemConfigBuilder();

  private final static String FACTORY_KEY = FileSystemConfigBuilder.class.getName() + ".KEY";

  private final static String PASSIVE_MODE = FileSystemConfigBuilder.class.getName() + ".PASSIVE";

  private final static String USER_DIR_IS_ROOT = FileSystemConfigBuilder.class.getName() + ".USER_DIR_IS_ROOT";

  private final static String DATA_TIMEOUT = FileSystemConfigBuilder.class.getName() + ".DATA_TIMEOUT";

  public static GsiFtpFileSystemConfigBuilder getInstance( )
  {
    return builder;
  }

  private GsiFtpFileSystemConfigBuilder( )
  {
  }

  /**
   * set the FQCN of your FileEntryParser used to parse the directory listing from your server.<br />
   * <br />
   * <i>If you do not use the default commons-net FTPFileEntryParserFactory e.g. by using {@link #setEntryParserFactory}
   * this is the "key" parameter passed as argument into your custom factory</i>
   * 
   * @param opts
   * @param key
   */
  public void setEntryParser( FileSystemOptions opts, String key )
  {
    setParam( opts, FACTORY_KEY, key );
  }

  /**
   * @param opts
   * @see #setEntryParser
   */
  public String getEntryParser( FileSystemOptions opts )
  {
    return (String) getParam( opts, FACTORY_KEY );
  }

  /**
   * @see org.apache.commons.vfs.FileSystemConfigBuilder#getConfigClass()
   */
  @Override
  protected Class<GsiFtpFileSystem> getConfigClass( )
  {
    return GsiFtpFileSystem.class;
  }

  /**
   * enter into passive mode
   * 
   * @param opts
   * @param passiveMode
   */
  public void setPassiveMode( FileSystemOptions opts, boolean passiveMode )
  {
    setParam( opts, PASSIVE_MODE, passiveMode ? Boolean.TRUE : Boolean.FALSE );
  }

  /**
   * @param opts
   * @see #setPassiveMode
   */
  public Boolean getPassiveMode( FileSystemOptions opts )
  {
    return (Boolean) getParam( opts, PASSIVE_MODE );
  }

  /**
   * use user directory as root (do not change to fs root)
   * 
   * @param opts
   * @param userDirIsRoot
   */
  public void setUserDirIsRoot( FileSystemOptions opts, boolean userDirIsRoot )
  {
    setParam( opts, USER_DIR_IS_ROOT, userDirIsRoot ? Boolean.TRUE : Boolean.FALSE );
  }

  /**
   * @param opts
   * @see #setUserDirIsRoot
   */
  public Boolean getUserDirIsRoot( FileSystemOptions opts )
  {
    return (Boolean) getParam( opts, USER_DIR_IS_ROOT );
  }

  /**
   * @param opts
   * @see #setDataTimeout
   */
  public Integer getDataTimeout( FileSystemOptions opts )
  {
    return (Integer) getParam( opts, DATA_TIMEOUT );
  }

  /**
   * set the data timeout for the ftp client.<br />
   * If you set the dataTimeout to <code>null</code> no dataTimeout will be set on the ftp client.
   * 
   * @param opts
   * @param dataTimeout
   */
  public void setDataTimeout( FileSystemOptions opts, Integer dataTimeout )
  {
    setParam( opts, DATA_TIMEOUT, dataTimeout );
  }

  public void setCredential( GSSCredential credential, FileSystemOptions opts )
  {
    setParam( opts, "credential", credential );
  }

  public GSSCredential getCredential( FileSystemOptions opts )
  {
    return (GSSCredential) getParam( opts, "credential" );
  }

}
