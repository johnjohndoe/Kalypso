/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree.tools;

/**
 * The interface describes the access to a cache.
 * <p>
 * ---------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface Cache
{

  /**
   * sets the number of entries maximal allowed within the cache
   */
  public void setMaxEntries( int maxEntries );

  /**
   * returns the number of entries maximal allowed within the cache
   */
  public int getMaxEntries();

  /**
   * sets the maximal time (milliseconds) an entry will be stored within the cache without being called. If an entry is
   * called through the get-method the timestamp will be refreshed.
   */
  public void setMaxLifeTime( int maxLifeTime );

  /**
   * sets the maximal time (milliseconds) an entry will be stored within the cache without being called.
   */
  public int getMaxLifeTime();

  /**
   * returns the actual amount of entries within the cache
   */
  public int getCurrentSize();

  /**
   * removes all entries from the cache
   */
  public void clear();

  /**
   * pushes a new entry to the cache. Each entry is marked by an unique identifier
   */
  public void push( Object identifier, Object data );

  /**
   * gets an entry from the cache. The entry that shall be returned will be identified by the submitted identifier. If
   * no entry for a submitted identifier can be found within the cache the method shall return <tt>null</tt>.
   */
  public Object get( Object identifier );

  /**
   * removes an entry from the cache. The entry that shall be removed will be identified by the submitted identifier.
   * The removed entry will be returned. If no entry for a submitted identifier can be found within the cache the method
   * shall return <tt>null</tt>.
   */
  public Object remove( Object identifier );

}