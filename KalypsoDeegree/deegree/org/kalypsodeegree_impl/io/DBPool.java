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
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Properties;

import org.kalypsodeegree_impl.tools.ObjectPool;

/**
 * class to manage a pool of database connections.
 * 
 * <p>
 * ----------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 07.02.2001
 *          <p>
 */
public class DBPool extends ObjectPool
{

  private String driver = null;

  private String database = null;

  private Properties properties = new Properties();

  // private constructor to protect initializing
  public DBPool( final String driver, final String database, final String user,
      final String password )
  {

    this.driver = driver;
    this.database = database;
    properties.put( "user", user );
    properties.put( "password", password );
  }

  // private constructor to protect initializing
  public DBPool( final String driver, final String database, final Properties properties )
  {

    this.driver = driver;
    this.database = database;
    this.properties = properties;
  }

  /**
   * get an object from the object pool
   */
  public synchronized Object acuireObject() throws Exception
  {

    // if the maximum amount of instances are in use
    // wait until an instance has been released back
    // to the pool or 20 seconds has passed
    long timediff = 0;
    while( in_use.size() == getMaxInstances() && timediff < 20000 )
    {
      Thread.sleep( 100 );
      timediff += 100;
    }
    // if no instance has been released within 20 seconds
    // or can newly be instantiated return null
    if( timediff >= 20000 )
      return null;

    // if a none used is available from the pool
    if( available.size() > 0 )
    {

      // get/remove ojebct from the pool
      Object o = available.remove( available.size() - 1 );
      if( ( (Connection)o ).isClosed() )
      {
        o = acuireObject();
      }

      // add it to 'in use' container
      in_use.add( o );

      // reset its start life time
      startLifeTime.put( o, new Long( System.currentTimeMillis() ) );
      // set the start of its usage
      startUsageTime.put( o, new Long( System.currentTimeMillis() ) );

      // return the object
      return o;

    }
    // else instatiate a new object
    // create a new class instance
    DriverManager.registerDriver( (Driver)Class.forName( driver ).newInstance() );

    Properties prop = (Properties)properties.clone();
    Object o = DriverManager.getConnection( database, prop );

    existingInstances++;

    // add it to 'in use' container
    in_use.add( o );
    // set the start of its life time
    startLifeTime.put( o, new Long( System.currentTimeMillis() ) );
    // set the start of its usage
    startUsageTime.put( o, new Long( System.currentTimeMillis() ) );

    // return the object
    return o;
  }

  /**
   * will be called when the object is removed from the pool
   */
  public void onObjectKill( Object o )
  {
    try
    {
      ( (Connection)o ).close();
    }
    catch( SQLException e )
    {
        // shouldnt we print stack trace?
    }
  }

}