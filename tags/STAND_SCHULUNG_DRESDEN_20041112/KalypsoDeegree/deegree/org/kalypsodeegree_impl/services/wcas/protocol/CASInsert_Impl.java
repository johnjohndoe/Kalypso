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

import org.deegree.services.wcas.protocol.CASInsert;

/**
 * An insert opration adds new metadata to the catalog. because a catalog
 * probably knows several different metadata fromats no specific type is used to
 * access the metadata.
 * <p>
 * --------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-04-16
 */
final public class CASInsert_Impl extends CASOperation_Impl implements CASInsert
{
  private ArrayList metadata = null;

  /**
   * Creates a new CASInsert_Impl object.
   * 
   * @param handle
   * @param metadata
   */
  CASInsert_Impl( String handle, Object[] metadata )
  {
    super( handle );
    this.metadata = new ArrayList();
    setMetadata( metadata );
  }

  /**
   * returns the metadata that shall be added to a catalog
   */
  public Object[] getMetadata()
  {
    return metadata.toArray();
  }

  /**
   * @see CASInsert_Impl#getMetadata()
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
   * @see CASInsert_Impl#getMetadata()
   */
  public void addMetadata( Object metadata )
  {
    this.metadata.add( metadata );
  }
}