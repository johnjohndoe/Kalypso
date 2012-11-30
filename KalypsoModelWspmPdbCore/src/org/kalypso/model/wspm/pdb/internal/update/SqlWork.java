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

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;

import org.hibernate.jdbc.Work;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class SqlWork implements Work
{
  private final String[] m_sqls;

  public SqlWork( final String[] sqls )
  {
    m_sqls = sqls;
  }

  @Override
  public String toString( )
  {
    return Messages.getString( "SqlWork.0" ); //$NON-NLS-1$
  }

  @Override
  public void execute( final Connection connection ) throws SQLException
  {
    final boolean oldAutoCommit = connection.getAutoCommit();

    try
    {
      connection.setAutoCommit( false );
      connection.commit();
      executeScripts( connection );
      connection.commit();
    }
    catch( final SQLException e )
    {
      // Rollback etc. does not work, why?
      connection.rollback();
      throw e;
    }
    catch( final Throwable e )
    {
      connection.rollback();
      throw new SQLException( e );
    }
    finally
    {
      connection.setAutoCommit( oldAutoCommit );
    }
  }

  private void executeScripts( final Connection connection ) throws SQLException
  {
    for( final String sql : m_sqls )
    {
      try
      {
        final Statement statement = connection.createStatement();
        statement.execute( sql );
        statement.close();
      }
      catch( final SQLException e )
      {
        System.err.println( "Failed to execute sql: " ); //$NON-NLS-1$
        System.err.println( sql );
        System.err.println();
        throw e;
      }
    }
  }
}