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

import org.apache.commons.vfs.FileSystemException;
import org.apache.commons.vfs.FileSystemOptions;
import org.globus.ftp.GridFTPClient;
import org.globus.ftp.exception.ServerException;
import org.ietf.jgss.GSSCredential;

/**
 * Create a GridFtpClient instance
 * 
 * @author <a href="mailto:imario@apache.org">Mario Ivankovits</a>
 * @author <a href="mailto:vladimir_silva@yaho.com">Vladimir Silva</a>
 * @version $Revision$ $Date: 2005-10-14 10:59:47 -0700 (Fri, 14 Oct 2005) $
 */
public class GsiFtpClientFactory {

	private GsiFtpClientFactory() {
	}

	/**
	 * Creates a new connection to the server.
	 */
	public static GridFTPClient createConnection(final String hostname,
			final int port, final FileSystemOptions fileSystemOptions)
			throws FileSystemException {
		// if ( password == null)
		// throw new Exception("Password cannot be null");

		// Create a proxy cert (if missing)
		// new ProxyTool().createProxy(password);

		GridFTPClient client = null;
		try {
			client = new GridFTPClient(hostname, port);
			// Authenticate w/ user credentials defines in
			// $HOME/.globus/cog.properties
			// client.authenticate(null);
			GSSCredential credential = GsiFtpFileSystemConfigBuilder
					.getInstance().getCredential(fileSystemOptions);
			client.authenticate(credential);
		} catch (final ServerException e1) {
			throw new FileSystemException(e1);
		} catch (final IOException e1) {
			throw new FileSystemException(e1);
		}

		// Set binary mode
		// if (!client.setFileType(FTP.BINARY_FILE_TYPE))
		// {
		// throw new
		// FileSystemException("vfs.provider.ftp/set-binary.error",
		// hostname);
		// }

		// Set dataTimeout value
		// Integer dataTimeout =
		// FtpFileSystemConfigBuilder.getInstance().getDataTimeout(fileSystemOptions);
		// if (dataTimeout != null)
		// {
		// client.setDataTimeout(dataTimeout.intValue());
		// }

		// Change to root by default
		// All file operations a relative to the filesystem-root
		// String root = getRoot().getName().getPath();

		// Boolean userDirIsRoot =
		// FtpFileSystemConfigBuilder.getInstance().getUserDirIsRoot(fileSystemOptions);
		// if (workingDirectory != null && (userDirIsRoot == null ||
		// !userDirIsRoot.booleanValue()))
		// {
		// if (!client.changeWorkingDirectory(workingDirectory))
		// {
		// throw new
		// FileSystemException("vfs.provider.ftp/change-work-directory.error",
		// workingDirectory);
		// }
		// }
		//
		// Boolean passiveMode =
		// FtpFileSystemConfigBuilder.getInstance().getPassiveMode(fileSystemOptions);
		// if (passiveMode != null && passiveMode.booleanValue())
		// {
		// client.enterLocalPassiveMode();
		// }

		return client;
	}
}