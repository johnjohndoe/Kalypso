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
package org.deegree_impl.services.wcas.protocol;

import java.util.ArrayList;

import org.deegree.services.wcas.protocol.CASUpdate;
import org.deegree.services.wfs.filterencoding.Filter;

/**
 * The update operation changes the content of the metadata entries identified
 * by the <tt>Filter</tt>
 * <p>
 * --------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-04-16
 */
final public class CASUpdate_Impl extends CASOperation_Impl implements CASUpdate
{
  private ArrayList metadata = null;

  private Filter filter = null;

  /**
   * Creates a new CASUpdate_Impl object.
   * 
   * @param handle
   * @param filter
   * @param metadata
   */
  CASUpdate_Impl( String handle, Filter filter, Object[] metadata )
  {
    super( handle );
    this.metadata = new ArrayList();
    setFilter( filter );
    setMetadata( metadata );
  }

  /**
   * returns the metadata that shall be updated. because a catalog may supports
   * more than one data format, no specific type is definied.
   */
  public Object[] getMetadata()
  {
    return this.metadata.toArray();
  }

  /**
   * @see CASUpdate_Impl#getMetadata()
   */
  public void setMetadata( Object[] metadata )
  {
    this.metadata.clear();

    if( metadata != null )
    {
      for( int i = 0; i < metadata.length; i++ )
      {
        addMetadata( metadata[i] );
      }
    }
  }

  /**
   * @see CASUpdate_Impl#getMetadata()
   */
  public void addMetadata( Object metadata )
  {
    this.metadata.add( metadata );
  }

  /**
   * returns the <tt>Filter</tt> that describes which records shall be changed -
   * updated.
   */
  public Filter getFilter()
  {
    return filter;
  }

  /**
   * @see CASUpdate_Impl#getFilter()
   */
  public void setFilter( Filter filter )
  {
    this.filter = filter;
  }
}