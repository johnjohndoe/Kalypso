
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

import org.deegree.services.wfs.Dispatcher;
import org.deegree.services.wfs.capabilities.WFSCapabilities;
import org.deegree_impl.services.wfs.WFSFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.ObjectPool;


/**
 * class to manage the object pool. this is part
 * of the combination of the object pool pattern an the singelton
 * pattern.
 * <p>----------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 07.02.2001 
 * <p>
 */
public class WFSServicePool extends ObjectPool {
    
    private static WFSServicePool instance 	= null;
    private WFSCapabilities capa		= null;
    private Dispatcher dispatcher 		= null;
    
    // private constructor to protect initializing
    private WFSServicePool(WFSCapabilities capa) {        
        this.capa = capa;
        super.setMaxInstances( 30 );
    }
    
    
    /**
     * realize singelton pattern using double checked locking pattern.
     *
     * @return an instance of the object pool. it is gauranteed that
     *			there exists only one instance of pool for each submitted
     *			class name.
     */
    public static WFSServicePool getInstance(WFSCapabilities capa) {
        if (instance == null) {
            synchronized(WFSServicePool.class) {
                if (instance == null) {
                    instance = new WFSServicePool( capa );
                }
            }
        }
        return instance;
    }
    
    public void destroy() {
        clear();
        instance = null;
    }
    
    public synchronized Object acuireObject() throws Exception {
        
        Debug.debugMethodBegin( this, "acuireObject" );
                
        // if the maximum amount of instances are in use
        // wait until an instance has been released back
        // to the pool or 20 seconds has passed
        long timediff = 0;
   
        while (in_use.size() == getMaxInstances() && timediff < 30000) {
            Thread.sleep(200);
            timediff += 100;
        }
        // if no instance has been released within 20 seconds
        // or can newly be instantiated return null
        if (timediff >= 30000) {
            return null;
        }
        
        // if a none used is available from the pool
        if (available.size() > 0) {
            
            // get/remove ojebct from the pool
            Object o = available.get( available.size()-1 );
            available.remove( o );
            // add it to 'in use' container
            in_use.add( o );
            // reset its start life time
            startLifeTime.put( o, new Long( System.currentTimeMillis() ) );
            // set the start of its usage
            startUsageTime.put( o, new Long( System.currentTimeMillis() ) );
            
            Debug.debugMethodEnd();
            // return the object
            return o;
            
        } else
            // else instatiate a new object
        {
            // create a new class instance
            Object o = WFSFactory.createWFSService( capa, dispatcher );
            
            existingInstances++;
            
            // add it to 'in use' container
            in_use.add( o );
            // set the start of its life time
            startLifeTime.put( o, new Long( System.currentTimeMillis() ) );
            // set the start of its usage
            startUsageTime.put( o, new Long( System.currentTimeMillis() ) );
            
            Debug.debugMethodEnd();
            // return the object
            return o;
            
        }
        
    }
    
    /**
     * fill the pool with the submitted number of instances
     */
    public void fill(int noOfInstances) throws Exception {
        for (int i = 0; i < noOfInstances; i++) {
            Object o = WFSFactory.createWFSService( capa, dispatcher );
            existingInstances++;
            available.add( o );
            // set the start of its life time
            startLifeTime.put( o, new Long( System.currentTimeMillis() ) );
        }
    }
    
    /** 
     * this method will be called when the submitted object
     * will be removed from the pool
     */
    public void onObjectKill(Object o) {
    } 
    
    
    public String toString() {
        String ret = "super: \n" + super.toString() + "\n";
        ret += "---------------------------";
        ret += getClass().getName() + "\n";
        ret += "instance = " + instance + "\n";
        ret += "capa = " + capa + "\n";
        return ret;
    }
    
    
    
    
}
