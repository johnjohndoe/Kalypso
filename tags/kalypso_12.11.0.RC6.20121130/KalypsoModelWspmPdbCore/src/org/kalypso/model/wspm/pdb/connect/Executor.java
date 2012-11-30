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
package org.kalypso.model.wspm.pdb.connect;

import java.sql.SQLException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.hibernate.HibernateException;
import org.hibernate.JDBCException;
import org.hibernate.Session;
import org.hibernate.Transaction;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;

import com.vividsolutions.jts.util.Assert;

/**
 * @author Gernot Belger
 */
public class Executor
{
  private final Session m_session;

  private final IPdbOperation m_operation;

  public Executor( final Session session, final IPdbOperation operation )
  {
    m_session = session;
    m_operation = operation;
  }

  public String getLabel( )
  {
    return m_operation.getLabel();
  }

  public void execute( ) throws PdbConnectException
  {
    execute( null );
  }

  public IStatus execute( final IProgressMonitor monitor ) throws PdbConnectException
  {
    Assert.isTrue( m_session.isOpen() );

    Transaction transaction = null;
    try
    {
      transaction = m_session.beginTransaction();

      executeOperation( m_session, m_operation, monitor );

      transaction.commit();

      if( m_operation instanceof IPdbOperationWithMonitor )
        return ((IPdbOperationWithMonitor)m_operation).getStatus();

      return Status.OK_STATUS;
    }
    catch( final Throwable e )
    {
      doRollback( transaction );

      final PdbConnectException pce = logError( e );

      throw pce;
    }
  }

  private void executeOperation( final Session session, final IPdbOperation operation, final IProgressMonitor monitor ) throws PdbConnectException
  {
    if( operation instanceof IPdbOperationWithMonitor )
      ((IPdbOperationWithMonitor)operation).setMonitor( monitor );
    else if( monitor != null )
      monitor.beginTask( operation.getLabel(), IProgressMonitor.UNKNOWN );

    operation.execute( session );

    if( monitor != null )
      monitor.done();
  }

  private PdbConnectException logError( final Throwable e )
  {
    if( e instanceof HibernateException )
      return logHibernateException( (HibernateException)e );
    else
    {
      e.printStackTrace();
      final String message = String.format( Messages.getString( "Executor_0" ), m_operation.getLabel() ); //$NON-NLS-1$
      return new PdbConnectException( message, e );
    }
  }

  private PdbConnectException logHibernateException( final HibernateException e )
  {
    if( e instanceof JDBCException )
    {
      final JDBCException je = (JDBCException)e;
      final SQLException sqlException = je.getSQLException();
      logSQLException( sqlException );
    }

    e.printStackTrace();

    final String message = String.format( Messages.getString( "Executor_0" ), m_operation.getLabel() ); //$NON-NLS-1$
    return new PdbConnectException( message, e );
  }

  private void logSQLException( final SQLException sqlException )
  {
    SQLException nextException = sqlException;
    while( nextException != null )
    {
      System.out.println( nextException.getMessage() );
      nextException = nextException.getNextException();
    }
  }

  private void doRollback( final Transaction transaction ) throws PdbConnectException
  {
    try
    {
      if( transaction != null )
        transaction.rollback();
    }
    catch( final HibernateException e )
    {
      e.printStackTrace();
      throw new PdbConnectException( Messages.getString( "Executor_2" ), e ); //$NON-NLS-1$
    }
  }
}
