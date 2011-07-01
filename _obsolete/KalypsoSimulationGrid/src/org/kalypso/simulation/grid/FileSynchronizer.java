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
package org.kalypso.simulation.grid;

import java.io.IOException;

import org.apache.commons.vfs.FileObject;
import org.apache.commons.vfs.FileType;
import org.eclipse.core.runtime.Assert;
import org.kalypso.commons.io.VFSUtilities;

/**
 * This class helps synchronizing two directories called local and remote.
 * 
 * @author kurzbach
 * 
 */
public class FileSynchronizer {

	private final FileObject m_localDirectory;
	private final FileObject m_remoteDirectory;

	public FileSynchronizer(final FileObject localDirectory,
			final FileObject remoteDirectory) throws IOException {
		Assert.isLegal(localDirectory != null
				&& FileType.FOLDER.equals(localDirectory.getType()));
		Assert.isLegal(remoteDirectory != null
				&& FileType.FOLDER.equals(remoteDirectory.getType()));

		this.m_localDirectory = localDirectory;
		this.m_remoteDirectory = remoteDirectory;
	}

	/**
	 * Updates the files in the local directory to match the remote directory
	 * 
	 * @throws IOException
	 */
	public void updateLocal() throws IOException {
		VFSUtilities.copy(m_remoteDirectory, m_localDirectory, false);
	}

	/**
	 * Updates the files in the remote directory to match the local directory
	 * 
	 * @throws IOException
	 */
	public void updateRemote() throws IOException {
		VFSUtilities.copy(m_localDirectory, m_remoteDirectory, false);
	}
}
