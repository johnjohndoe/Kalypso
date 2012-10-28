/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Bj�rnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universit�t Hamburg-Harburg, Institut f�r Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.pdb.connect;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;

/**
 * Executes an {@link IPdbOperation} on a connection.
 * 
 * @author Gernot Belger
 */
public class PdbExecutorOperation implements ICoreRunnableWithProgress
{
  private final IPdbOperation m_operation;

  private final IPdbConnection m_connection;

  private final String m_errorMessage;

  public PdbExecutorOperation( final IPdbConnection connection, final IPdbOperation operation, final String errorMessage )
  {
    m_connection = connection;
    m_operation = operation;
    m_errorMessage = errorMessage;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    Session session = null;

    try
    {
      session = m_connection.openSession();

      final Executor executor = new Executor( session, m_operation );
      final IStatus status = executor.execute( monitor );

      session.close();

      return status;
    }
    catch( final HibernateException e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, m_errorMessage, e );
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, m_errorMessage, e );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, m_errorMessage, e ); //$NON-NLS-1$
    }
    finally
    {
      monitor.done();
    }
  }
}