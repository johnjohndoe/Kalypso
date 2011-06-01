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

import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.hibernate.Transaction;

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
    Assert.isTrue( m_session.isOpen() );

    Transaction transaction = null;
    try
    {
      transaction = m_session.beginTransaction();
      m_operation.execute( m_session );
      transaction.commit();
    }
    catch( final Throwable e )
    {
      e.printStackTrace();

      doRollback( transaction );

      final String message = String.format( "Failed to execute command: %s", m_operation.getLabel() );
      throw new PdbConnectException( message, e );
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
      throw new PdbConnectException( "Failed to rollback transaction", e );
    }
  }
}
