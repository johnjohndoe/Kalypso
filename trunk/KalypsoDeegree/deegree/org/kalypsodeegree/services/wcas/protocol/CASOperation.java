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

/**
 * this is the basic interface of all catalog operations (insert, delete,
 * update):
 * <p>--------------------------------------------------------------------</p>
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 2002-04-16
 */
public interface CASOperation {
	
	/**
	 * The &lt;InsertFeature&gt; element is used to indicate that the WFS is capable
	 * of creating new instances of a feature type.
	 */
	public static final int OPERATION_INSERT = 0;

	/**
	 * The &lt;UpdateFeature&gt; element indicates that the WFS can change the
	 * existing state of a feature.
	 */
	public static final int OPERATION_UPDATE = 1;
	
	/**
	 * The &lt;DeleteFeature&gt; element indicates that the WFS can delete or
	 * remove instances of a feature type from the datastore.
	 */	
	public static final int OPERATION_DELETE = 2;

	/**
	 * The &lt;QueryFeature&gt; element indicates that the WFS is capable of
	 * executing a query on a feature type.
	 */	
	public static final int OPERATION_QUERY = 3;

	/**
	 * The &lt;LockFeature&gt; element indicates that the WFS is capable of
	 * locking instances of a feature type.
	 */	
	public static final int OPERATION_LOCK = 4;

   /**
    * handle is an useful variable to submit additional informations
    * to the catalog (service).
    */
    String getHandle();

}
