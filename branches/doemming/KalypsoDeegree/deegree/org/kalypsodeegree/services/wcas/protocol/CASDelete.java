/*----------------    FILE HEADER  ------------------------------------------

This file is part of deegree (Java Framework for Geospatial Solutions).
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
package org.deegree.services.wcas.protocol;

import org.deegree.services.wfs.filterencoding.Filter;

/**
 * The delete operation removes zero to n metadata entries from a 
 * catalog that matches the <tt>Filter</tt> that is returned by the
 * methode <tt>getFilter</tt>. If no <tt>Filter</tt> is defined all
 * entries will be removed from the data-metadata or service metadata
 * catalog (@see getType).
 * <p>--------------------------------------------------------------------</p>
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 2002-04-16
 */
public interface CASDelete extends CASOperation {
	
   /**
    * returns the <tt>Filter</tt> that defines which entries shall
    * be removed from the catalog.    
    */		
	Filter getFilter();
	
   /**
    * possible return values are 'ISO19115' and 'ISO19119' indicating
    * if the entries that matches the <tt>Filter</tt> shall be removed
    * from the data-metadata or the service-metadata.
    */	
    String getType();
}
