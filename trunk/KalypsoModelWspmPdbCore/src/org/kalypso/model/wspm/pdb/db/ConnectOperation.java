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
package org.kalypso.model.wspm.pdb.db;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;

/**
 * @author Gernot Belger
 */
public class ConnectOperation implements ICoreRunnableWithProgress
{
  private final IPdbSettings m_settings;

  private IPdbConnection m_connection;

  public ConnectOperation( final IPdbSettings settings )
  {
    m_settings = settings;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final String taskName = String.format( "Connecting to %s", m_settings.getName() );
    monitor.beginTask( taskName, IProgressMonitor.UNKNOWN );

    monitor.subTask( "connecting..." );

    try
    {
      m_connection = m_settings.createConnection();
      m_connection.connect();

      monitor.subTask( "checking database version..." );
      final PdbInfo info = m_connection.getInfo();
      /* final String version = */info.getVersion();
      // TODO: check if version is the current one
      return Status.OK_STATUS;
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();

      /* Try to close the connection */
// PdbUtils.closeQuietly( m_connection );

      // TODO: make message more human readable
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, "Connection failed", e );

      throw new CoreException( status );
    }
  }

  public IPdbConnection getConnection( )
  {
    return m_connection;
  }
}
