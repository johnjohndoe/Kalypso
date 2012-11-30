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
package org.kalypso.model.wspm.pdb.internal.update;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.hibernate.Session;
import org.hibernate.jdbc.Work;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;

/**
 * @author Gernot Belger
 */
public class WorkRunnable implements ICoreRunnableWithProgress
{
  private final IStatus m_okStatus = Status.OK_STATUS;

  private final IPdbConnection m_connection;

  private final Work m_work;

  public WorkRunnable( final IPdbConnection connection, final Work work )
  {
    m_connection = connection;
    m_work = work;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    Session session = null;

    try
    {
      monitor.beginTask( m_work.toString(), IProgressMonitor.UNKNOWN );

      session = m_connection.openSession();

      session.doWork( m_work );

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