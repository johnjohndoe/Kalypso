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
package org.deegree_impl.clients.wcasclient.model;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class ModelList
{
  protected HashMap list = null;

  public ModelList()
  {
    list = new HashMap();
  }

  /**
   * Creates a new CatalogEntryList object with an initial size
   * 
   * @param initialSize
   */
  public ModelList( int initialSize )
  {
    list = new HashMap( initialSize );
  }

  /**
   * adds an entry (MD_Metadata) to the list
   * 
   * @param entry
   *          a base MD_Metadata description
   */
  public void addEntry( BaseMetadata entry )
  {
    list.put( entry.getFileIdentifier(), entry );
  }

  /**
   * returns an entry (MD_Metadata object) identified by the passed id
   * 
   * @param id
   * 
   * @return base MD_Metadata object
   */
  public Object getEntry( String id )
  {
    return list.get( id );
  }

  /**
   * returns all base MD_Metadata objects
   * 
   * @return all base MD_Metadata objects
   */
  public Object[] getAll( Object[] ob )
  {
    return list.values().toArray( ob );
  }

  /**
   * returns all base MD_Metadata objects sorted by their names
   * 
   * @return
   */
  public Object[] getAllSortedByName( Object[] ob )
  {
    Object[] entries = getAll( ob );
    Arrays.sort( entries, new ComparatorImpl() );

    return entries;
  }

  /**
   * removes an entry identified by its id from the list
   * 
   * @param id
   * 
   * @return
   */
  public Object removeEntry( String id )
  {
    return list.remove( id );
  }

  /**
   * clears the list
   */
  public void clear()
  {
    list.clear();
  }

  /**
   * 
   * 
   * @version $Revision$
   * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
   */
  protected class ComparatorImpl implements Comparator
  {
    /**
     * 
     * 
     * @param o1
     * @param o2
     * 
     * @return
     */
    public int compare( Object o1, Object o2 )
    {
      String s1 = o1.toString();
      String s2 = o1.toString();
      return s1.compareTo( s2 );
    }
  }
}