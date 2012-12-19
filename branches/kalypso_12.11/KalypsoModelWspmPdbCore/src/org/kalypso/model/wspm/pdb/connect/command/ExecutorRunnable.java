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
package org.kalypso.model.wspm.pdb.connect.command;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.hibernate.Session;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.Executor;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;

/**
 * @author Gernot Belger
 * @deprecated Use {@link org.kalypso.model.wspm.pdb.connect.PdbExecutorOperation} instead.
 */
@Deprecated
public class ExecutorRunnable implements ICoreRunnableWithProgress
{
  private IStatus m_okStatus = Status.OK_STATUS;

  private final IPdbConnection m_connection;

  private final IPdbOperation m_operation;

  public ExecutorRunnable( final IPdbConnection connection, final IPdbOperation operation )
  {
    m_connection = connection;
    m_operation = operation;
  }

  /**
   * Sets an OK status that will be returned in case of success.
   */
  public void setOKStatus( final Status status )
  {
    m_okStatus = status;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    Session session = null;

    try
    {
      session = m_connection.openSession();

      final Executor m_executor = new Executor( session, m_operation );
      m_executor.execute( monitor );

      session.flush();
      session.close();

      return m_okStatus;
    }
    catch( final PdbConnectException e )
    {
      throw new InvocationTargetException( e );
    }
    finally
    {
      PdbUtils.closeSessionQuietly( session );
    }
  }
}