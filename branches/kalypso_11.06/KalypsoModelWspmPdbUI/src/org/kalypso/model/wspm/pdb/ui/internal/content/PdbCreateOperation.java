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

import java.io.IOException;

import org.hibernate.HibernateException;
import org.hibernate.SQLQuery;
import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.version.UpdateScript;
import org.kalypso.model.wspm.pdb.db.version.UpdateScriptExtenions;
import org.osgi.framework.Version;

/**
 * @author Gernot Belger
 */
public class PdbCreateOperation implements IPdbOperation
{
  private final Version m_sourceVersion = new Version( 0, 0, 0 );

  private final String m_dbType;

  public PdbCreateOperation( final String dbType )
  {
    m_dbType = dbType;
  }

  @Override
  public String getLabel( )
  {
    return "Creating database (this may take several minutes)....";
  }

  @Override
  public void execute( final Session session ) throws PdbConnectException
  {
    try
    {
      executeUpdateScripts( session );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new PdbConnectException( "Failed to update database", e );
    }
    catch( final HibernateException e )
    {
      e.printStackTrace();
      throw new PdbConnectException( "Failed to update database", e );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new PdbConnectException( "Failed to update database", e );
    }
  }

  private void executeUpdateScripts( final Session session ) throws IOException
  {
    final UpdateScript script = UpdateScriptExtenions.getScript( m_sourceVersion, m_dbType );
    final String sql = script.loadSQL();
    final SQLQuery sqlQuery = session.createSQLQuery( sql );
    sqlQuery.executeUpdate();
  }
}