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
package org.deegree.services.wfs.protocol;

import org.deegree.services.wfs.filterencoding.*;


/**
* Each individual query packaged in a GetFeature request is defined 
* using the query value. The query value defines which feature type 
* to query, what properties to retrieve and what constraints (spatial 
* and non-spatial) to apply to those properties.
*
* <p>--------------------------------------------------------</p>
*
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
* @version $Revision$ $Date$
*/
public interface WFSQuery {
    /**
     * The property names is used to enumerate the feature properties 
     * or attributes that should be selected. If no property names are 
     * specified then all properties should be fetched.
     */
    public String[] getPropertyNames();

    /**
     * The handle attribute is included to allow a client to associate a 
     * mnemonic name to the <Query> request. The purpose of the handle attribute 
     * is to provide an error handling mechanism for locating a statement that 
     * might fail.
     */
    public String getHandle();

    /**
     * The version attribute is included in order to accommodate systems that 
     * support feature versioning. A value of ALL indicates that all versions
     * of a feature should be fetched. Otherwise an integer can be specified 
     * to return the n th version of a feature. The version numbers start at '1' 
     * which is the oldest version. If a version value larger than the largest
     * version is specified then the latest version is return. The default action 
     * shall be for the query to return the latest version. Systems that do not
     * support versioning can ignore the parameter and return the only version 
     * that they have.
     */
    public String getVersion();

    /**
     * The typeName attribute is used to indicate the name of the feature type 
     * or class to be queried.
     */
    public String getTypeName();

    /**
     * returns the filter that limits the query
     */
    public Filter getFilter();

    /**
     * exports the <tt>WFSQuery</tt> as XML expression
     */
    String exportAsXML();
}

/*
 * Changes to this class. What the people haven been up to:
 *
 * $Log$
 * Revision 1.1  2004/05/11 16:43:22  doemming
 * Initial revision
 *
 * Revision 1.4  2004/02/09 07:57:02  poth
 * no message
 *
 * Revision 1.3  2003/04/23 07:23:15  poth
 * no message
 *
 * Revision 1.2  2003/04/10 07:31:06  poth
 * no message
 *
 * Revision 1.1.1.1  2002/09/25 16:01:54  poth
 * no message
 *
 * Revision 1.4  2002/08/15 10:02:41  ap
 * no message
 *
 * Revision 1.3  2002/04/26 09:02:51  ap
 * no message
 *
 * Revision 1.1  2002/04/25 16:17:20  ap
 * no message
 *
 * Revision 1.1  2002/04/04 16:17:15  ap
 * no message
 *

 */
