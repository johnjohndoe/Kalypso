/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.io;

import java.sql.Connection;
import java.util.HashMap;
import java.util.Properties;

/**
 * class to manage a database connection pool. this is part of the combination
 * of the object pool pattern an the singelton pattern.
 * <p>
 * ----------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 07.02.2001
 *          <p>
 */
public class DBConnectionPool
{
  private static DBConnectionPool instance = null;

  private HashMap pools = null;

  /**
   * Creates a new DBConnectionPool object.
   */
  private DBConnectionPool()
  {
    pools = new HashMap();
  }

  /**
   * realize singelton pattern using double checked locking pattern.
   * 
   * @return an instance of the data base pool. it is gauranteed that there
   *         exists only one instance of pool for each submitted class name.
   */
  public static DBConnectionPool getInstance()
  {
    if( instance == null )
    {
      synchronized( DBConnectionPool.class )
      {
        if( instance == null )
        {
          instance = new DBConnectionPool();
        }
      }
    }

    return instance;
  }

  /**
   * get an object from the object pool
   */
  public synchronized Connection acuireConnection( final String driver, final String database,
      final String user, final String password ) throws Exception
  {
    String q = driver + database + user + password;

    if( pools.get( q ) == null )
    {
      DBPool pool = new DBPool( driver, database, user, password );
      pools.put( q, pool );
      return (Connection)pool.acuireObject();
    }
    else
    {
      DBPool pool = (DBPool)pools.get( q );
      return (Connection)pool.acuireObject();
    }
  }

  /**
   * get an object from the object pool
   */
  public synchronized Connection acuireConnection( final String driver, final String database,
      final Properties properties ) throws Exception
  {
    String q = driver + database + properties.toString();

    if( pools.get( q ) == null )
    {
      DBPool pool = new DBPool( driver, database, properties );
      pools.put( q, pool );
      return (Connection)pool.acuireObject();
    }
    else
    {
      DBPool pool = (DBPool)pools.get( q );
      return (Connection)pool.acuireObject();
    }
  }

  /**
   * releases a connection back to the pool
   */
  public synchronized void releaseConnection( final Connection con, final String driver,
      final String database, final String user, final String password ) throws Exception
  {
    String q = driver + database + user + password;
    DBPool pool = (DBPool)pools.get( q );
    pool.releaseObject( con );
  }

  /**
   * releases a connection back to the pool
   */
  public synchronized void releaseConnection( final Connection con, final String driver,
      final String database, final Properties properties ) throws Exception
  {
    String q = driver + database + properties.toString();
    DBPool pool = (DBPool)pools.get( q );
    pool.releaseObject( con );
  }
}