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
package org.kalypso.model.wspm.pdb.ui.internal.content;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.progress.IProgressConstants2;
import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class RefreshContentJob extends Job
{
  private final IPdbConnection m_connection;

  private ConnectionInput m_input;

  private ElementSelector m_elementToSelect;

  public RefreshContentJob( final IPdbConnection connection )
  {
    super( Messages.getString( "RefreshContentJob.0" ) ); //$NON-NLS-1$

    setProperty( IProgressConstants2.KEEP_PROPERTY, Boolean.FALSE );
    setProperty( IProgressConstants2.NO_IMMEDIATE_ERROR_PROMPT_PROPERTY, Boolean.TRUE );

    m_connection = connection;
  }

  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    monitor.beginTask( getName(), IProgressMonitor.UNKNOWN );

    Session session = null;
    try
    {
      session = m_connection.openSession();
      m_input = new ConnectionInput( session );

      // FIXME: instead, use some join-fetch mechanism to load all water bodies, states and events at once.

      // REMARK: we need to leave the session open here, else lazy initialization of elements
      // will not work. The session is closed when the input is disposed.
      // session.close();
      return Status.OK_STATUS;
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, Messages.getString( "RefreshContentJob.1" ), e ); //$NON-NLS-1$
    }
    finally
    {
      // REMARK: se above
      // PdbUtils.closeSessionQuietly( session );
      monitor.done();
    }
  }

  public ConnectionInput getInput( )
  {
    return m_input;
  }

  public synchronized void setElementToSelect( final ElementSelector elementToSelect )
  {
    m_elementToSelect = elementToSelect;
  }

  public synchronized ElementSelector getElementToSelect( )
  {
    return m_elementToSelect;
  }
}