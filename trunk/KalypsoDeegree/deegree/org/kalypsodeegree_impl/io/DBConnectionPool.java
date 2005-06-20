/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 
 history:
 
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
 
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.io;

import java.sql.Connection;
import java.util.HashMap;
import java.util.Properties;

/**
 * class to manage a database connection pool. this is part of the combination of the object pool pattern an the
 * singelton pattern.
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
   * @return an instance of the data base pool. it is gauranteed that there exists only one instance of pool for each
   *         submitted class name.
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
  public synchronized Connection acuireConnection( final String driver, final String database, final String user,
      final String password ) throws Exception
  {
    String q = driver + database + user + password;

    if( pools.get( q ) == null )
    {
      DBPool pool = new DBPool( driver, database, user, password );
      pools.put( q, pool );
      return (Connection)pool.acuireObject();
    }
    DBPool pool = (DBPool)pools.get( q );
    return (Connection)pool.acuireObject();
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
    DBPool pool = (DBPool)pools.get( q );
    return (Connection)pool.acuireObject();
  }

  /**
   * releases a connection back to the pool
   */
  public synchronized void releaseConnection( final Connection con, final String driver, final String database,
      final String user, final String password ) throws Exception
  {
    String q = driver + database + user + password;
    DBPool pool = (DBPool)pools.get( q );
    pool.releaseObject( con );
  }

  /**
   * releases a connection back to the pool
   */
  public synchronized void releaseConnection( final Connection con, final String driver, final String database,
      final Properties properties ) throws Exception
  {
    String q = driver + database + properties.toString();
    DBPool pool = (DBPool)pools.get( q );
    pool.releaseObject( con );
  }
}