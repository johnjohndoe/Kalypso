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
package org.kalypso.model.wspm.pdb.db;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class OpenConnectionJob extends Job
{
  private final IPdbSettings m_settings;

  private IStatus m_connectionStatus;

  private final boolean m_closeConnection;

  private IPdbConnection m_connection;

  public OpenConnectionJob( final IPdbSettings settings, final boolean closeConnection )
  {
    super( Messages.getString( "OpenConnectionJob_0" ) ); //$NON-NLS-1$

    m_settings = settings;
    m_closeConnection = closeConnection;
  }

  public IStatus getConnectionStatus( )
  {
    return m_connectionStatus;
  }

  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    IPdbConnection connection = null;
    try
    {
      final ConnectOperation operation = new ConnectOperation( m_settings );
      final IStatus status = operation.execute( monitor );
      if( !status.isOK() )
      {
        m_connectionStatus = status;
        return Status.OK_STATUS;
      }

      connection = operation.getConnection();
      final PdbInfo info = connection.getInfo();

      if( m_closeConnection )
        connection.close();
      else
        m_connection = connection;

      if( info == null )
        m_connectionStatus = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, Messages.getString( "OpenConnectionJob_1" ) ); //$NON-NLS-1$
      else
        m_connectionStatus = info.getStatus();
    }
    catch( final Exception e )
    {
      m_connectionStatus = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, Messages.getString( "OpenConnectionJob_2" ), e ); //$NON-NLS-1$
    }
    finally
    {
      if( m_closeConnection )
        PdbUtils.closeQuietly( connection );
    }

    return Status.OK_STATUS;
  }

  public IPdbConnection getConnection( )
  {
    return m_connection;
  }
}