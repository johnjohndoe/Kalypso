/*----------------    FILE HEADER  ------------------------------------------
 
 This file is part of deegree
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
 E-Mail: fitzke@giub.uni-bonn.de
 
 ---------------------------------------------------------------------------*/

package org.deegree_impl.enterprise;

import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree_impl.services.wms.WMService_Impl;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.ObjectPool;

/**
 * Pool of WMService-Instances. Singleton.
 * 
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class WMServicePool extends ObjectPool
{

  /** the only instance of this class */
  private static WMServicePool instance = null;

  /** all WMServices of this pool conform to these capabilities */
  private WMSCapabilities capabilities = null;

  /**
   * Private constructor to realize the singleton pattern.
   * 
   * @param capabilities
   *          the capabilities the pool objects must conform to
   */
  private WMServicePool( WMSCapabilities capabilities ) throws Exception
  {
    super.setMaxInstances( 20 );
    Debug.debugMethodBegin( this, "WMServicePool" );
    this.capabilities = capabilities;
    Debug.debugMethodEnd();
  }

  /**
   * Returns the only instance of this class.
   * 
   * @return the only instance of WMServicePool
   */
  synchronized public static WMServicePool getInstance( WMSCapabilities capabilities )
      throws Exception
  {
    Debug.debugMethodBegin( "WMServicePool", "getInstance" );
    if( instance == null )
      instance = new WMServicePool( capabilities );
    Debug.debugMethodEnd();
    return instance;
  }

  /**
   * Clears the pool. Objects in use while the method has been called won't be
   * put back to the pool if released back through the <tt>releaseObject</tt>
   * method afterwards.
   */
  public void destroy()
  {
    clear();
    instance = null;
  }

  /**
   * Acquires an instance of WMService from the pool.
   * 
   * @return an instance of WMService
   */
  public synchronized Object acuireObject() throws Exception
  {

    Debug.debugMethodBegin( this, "acuireObject" );

    // if the all instances are in use wait until an instance has been
    // released back to the pool or 30 seconds have passed
    long time = System.currentTimeMillis();
    if( in_use.size() == getMaxInstances() )
    {
      synchronized( this )
      {
        wait( 30000 );
      }
    }
    time = System.currentTimeMillis() - time;
    // if no instance has been released within 30 seconds return null
    if( time >= 30000 )
      return null;

    Object o = null;
    // if an object is available
    if( available.size() > 0 )
    {
      // get and remove object from pool
      o = available.get( available.size() - 1 );
      available.remove( o );
    }
    else
    {
      // else instantiate a new object
      o = new WMService_Impl( capabilities );
      existingInstances++;
    }
    // add instance to 'in use' container
    in_use.add( o );
    // reset its start life time
    startLifeTime.put( o, new Long( System.currentTimeMillis() ) );
    // set the start of its usage
    startUsageTime.put( o, new Long( System.currentTimeMillis() ) );
    Debug.debugMethodEnd();
    return o;
  }

  /**
   * Fills the pool with the given number of instances.
   * 
   * @param noOfInstances
   *          number of instances to create
   */
  public void fill( int noOfInstances )
  {
    Debug.debugMethodBegin( this, "fill" );
    for( int i = 0; i < noOfInstances; i++ )
    {
      Object o = new WMService_Impl( capabilities );
      existingInstances++;
      available.add( o );
      // set the start of its life time
      startLifeTime.put( o, new Long( System.currentTimeMillis() ) );
    }
    Debug.debugMethodEnd();
  }

  /**
   * This method will be called when the submitted object is removed from the
   * pool. FIXME!!!
   * 
   * @param o
   *          the object to kill
   */
  public void onObjectKill( Object o )
  {}

  /**
   * Produces a textual representation of this object.
   */
  public String toString()
  {
    String ret = "super: \n" + super.toString() + "\n";
    ret += "---------------------------";
    ret += getClass().getName() + "\n";
    ret += "instance = " + instance + "\n";
    ret += "capabilities = " + capabilities + "\n";
    return ret;
  }
}