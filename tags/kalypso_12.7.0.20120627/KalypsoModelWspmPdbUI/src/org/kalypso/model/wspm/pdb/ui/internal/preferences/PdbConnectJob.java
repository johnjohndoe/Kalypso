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
package org.kalypso.model.wspm.pdb.ui.internal.preferences;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.IPdbSettings;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.PdbSettings;
import org.kalypso.model.wspm.pdb.db.OpenConnectionThreadedOperation;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class PdbConnectJob extends Job
{
  private final String m_settingsName;

  private IPdbConnection m_connection;

  public PdbConnectJob( final String settingsName )
  {
    super( Messages.getString( "PdbConnectJob.0" ) ); //$NON-NLS-1$

    m_settingsName = settingsName;
  }

  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    try
    {
      final IPdbSettings settings = PdbSettings.getSettings( m_settingsName );
      final OpenConnectionThreadedOperation operation = new OpenConnectionThreadedOperation( settings, false );
      operation.execute( monitor );
      m_connection = operation.getConnection();

      return Status.OK_STATUS;
    }
    catch( final PdbConnectException e )
    {
      return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, Messages.getString( "PdbConnectJob.1" ), e ); //$NON-NLS-1$
    }
    catch( final InterruptedException e )
    {
      return Status.CANCEL_STATUS;
    }
  }

  public IPdbConnection getConnection( )
  {
    return m_connection;
  }
}