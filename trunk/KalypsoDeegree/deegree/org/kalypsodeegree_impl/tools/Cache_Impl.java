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
package org.deegree_impl.tools;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;

import org.deegree.tools.Cache;

/**
 * This class provides a cache for feature collections created by the wms filter
 * services.
 * <p>
 * -----------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */

public class Cache_Impl extends TimerTask implements Cache
{

  protected Map data = null;

  protected Map lastCallTime = null;

  // min * sec * millisec. example: 5*60*1000 = 5 minutes
  private int maxLifeTime = 30 * 60 * 1000;

  // min * sec * millisec. example: 5*60*1000 = 5 minutes
  private int updateInterval = 60 * 1000;

  private int maxCacheSize = 100;

  public Cache_Impl()
  {
    this( 100, 30 * 60 * 1000 );
  }

  public Cache_Impl( int maxEntries )
  {
    this( maxEntries, 30 * 60 * 1000 );
    maxCacheSize = maxEntries;
  }

  public Cache_Impl( int maxEntries, int maxLifeTime )
  {
    data = Collections.synchronizedMap( new HashMap( maxEntries ) );
    lastCallTime = Collections.synchronizedMap( new HashMap( maxEntries ) );
    maxCacheSize = maxEntries;
    this.maxLifeTime = maxLifeTime;
    Timer timer = new Timer();
    timer.scheduleAtFixedRate( this, 10000, updateInterval );
  }

  /**
   * gets an entry from the cache. The entry that shall be returned will be
   * identified by the submitted identifier. If no entry for a submitted
   * identifier can be found within the cache the method shall return
   * <tt>null</tt>.
   *  
   */
  public Object get( Object identifier )
  {
    Debug.debugMethodBegin( this, "get" );

    Object o = null;

    synchronized( data )
    {
      o = data.get( identifier );
      if( o != null )
      {
        lastCallTime.put( identifier, new Long( System.currentTimeMillis() ) );
      }
    }

    Debug.debugMethodEnd();
    return o;
  }

  /**
   * pushes a new entry to the cache. Each entry is marked by an unique
   * identifier
   *  
   */
  public void push( Object identifier, Object data )
  {
    Debug.debugMethodBegin( this, "put" );

    synchronized( this.data )
    {
      this.data.put( identifier, data );
      lastCallTime.put( identifier, new Long( System.currentTimeMillis() ) );
      if( this.data.size() > maxCacheSize )
      {
        deleteOldest();
      }
    }

    Debug.debugMethodEnd();
  }

  /**
   * removes the feature collection from the cache that hasn't been called for
   * the longest time.
   */
  protected Object deleteOldest()
  {
    Debug.debugMethodBegin( this, "deleteOldest" );

    Object key = null;
    long time = System.currentTimeMillis();

    Iterator iterator = lastCallTime.keySet().iterator();

    while( iterator.hasNext() )
    {
      Object currentKey = iterator.next();
      long thisTime = ( (Long)lastCallTime.get( currentKey ) ).longValue();
      if( thisTime < time )
      {
        time = thisTime;
        key = currentKey;
      }
    }

    lastCallTime.remove( key );
    Object o = data.remove( key );

    Debug.debugMethodEnd();
    return o;
  }

  /**
   * returns the actual amount of entries within the cache
   *  
   */
  public int getCurrentSize()
  {
    return data.size();
  }

  /**
   * returns the maximal time a feature collection is stored within the cache
   * without being called
   */
  public int getMaxLifeTime()
  {
    return ( this.maxLifeTime );
  }

  /**
   * sets the maximal time a feature collection is stored within the cache
   * without being called
   */
  public void setMaxLifeTime( int maxLifeTime )
  {
    this.maxLifeTime = maxLifeTime;
  }

  /**
   * returns the time interval the internal thread is looking for feature
   * collection that exeeds the max life time
   */
  public int getUpdateInterval()
  {
    return ( this.updateInterval );
  }

  /**
   * sets the time interval the internal thread is looking for feature
   * collection that exeeds the max life time
   */
  public void setUpdateInterval( int updateInterval )
  {
    this.updateInterval = updateInterval;
  }

  /**
   * returns the number of entries maximal allowed within the cache
   *  
   */
  public int getMaxEntries()
  {
    return ( this.maxCacheSize );
  }

  /**
   * sets the maximal number of objects that will be stored within the cache. Is
   * the cache if completly filled and a new object shall be added the object
   * that hasn't been called for the longest time will be removed.
   */
  public void setMaxEntries( int maxCacheSize )
  {
    this.maxCacheSize = maxCacheSize;
  }

  /**
   * removes all entries from the cache
   *  
   */
  public void clear()
  {
    synchronized( data )
    {
      data.clear();
    }
  }

  /**
   * removes an entry from the cache. The entry that shall be removed will be
   * identified by the submitted identifier. The removed entry will be returned.
   * If no entry for a submitted identifier can be found within the cache the
   * method shall return <tt>null</tt>.
   *  
   */
  public Object remove( Object identifier )
  {
    Object o = null;
    synchronized( data )
    {
      o = data.remove( identifier );
    }
    return o;
  }

  public String toString()
  {
    String ret = null;
    ret += "lastCallTime = " + lastCallTime + "\n";
    ret += "maxLifeTime = " + maxLifeTime + "\n";
    ret += "updateInterval = " + updateInterval + "\n";
    ret += "maxCacheSize = " + maxCacheSize + "\n";
    return ret;
  }

  public void run()
  {
    ArrayList list = new ArrayList( 100 );
    try
    {
      synchronized( data )
      {
        Iterator iterator = data.keySet().iterator();
        long currentTime = System.currentTimeMillis();

        while( iterator.hasNext() )
        {
          Object key = iterator.next();
          Long lng = (Long)lastCallTime.get( key );
          if( Math.abs( lng.longValue() - currentTime ) > maxLifeTime )
          {
            list.add( key );
          }
        }
        for( int i = list.size() - 1; i >= 0; i-- )
        {
          Object o = list.remove( i );
          data.remove( o );
          lastCallTime.remove( o );
        }
      }
    }
    catch( Exception e )
    {
      //System.out.println("Cache - Cleaner: " + e);
    }
  }

}