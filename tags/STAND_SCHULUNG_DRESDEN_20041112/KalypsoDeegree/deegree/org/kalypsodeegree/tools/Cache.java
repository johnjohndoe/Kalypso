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
package org.deegree.tools;

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
   * sets the maximal time (milliseconds) an entry will be stored within the
   * cache without being called. If an entry is called through the get-method
   * the timestamp will be refreshed.
   */
  public void setMaxLifeTime( int maxLifeTime );

  /**
   * sets the maximal time (milliseconds) an entry will be stored within the
   * cache without being called.
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
   * pushes a new entry to the cache. Each entry is marked by an unique
   * identifier
   */
  public void push( Object identifier, Object data );

  /**
   * gets an entry from the cache. The entry that shall be returned will be
   * identified by the submitted identifier. If no entry for a submitted
   * identifier can be found within the cache the method shall return
   * <tt>null</tt>.
   */
  public Object get( Object identifier );

  /**
   * removes an entry from the cache. The entry that shall be removed will be
   * identified by the submitted identifier. The removed entry will be returned.
   * If no entry for a submitted identifier can be found within the cache the
   * method shall return <tt>null</tt>.
   */
  public Object remove( Object identifier );

}