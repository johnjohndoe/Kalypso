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

import org.deegree.services.wcas.protocol.CASQuery;
import org.deegree.services.wfs.filterencoding.Filter;

import org.deegree_impl.services.wfs.protocol.WFSQuery_Impl;


/**
 * 
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class CASQuery_Impl extends WFSQuery_Impl implements CASQuery {
    private String propertySetName = null;

    /**
     * Creates a new CASQuery_Impl object.
     *
     * @param propertyNames 
     * @param handle 
     * @param version 
     * @param typeName 
     * @param filter 
     * @param propertySetName 
     */
    CASQuery_Impl(String[] propertyNames, String handle, String version, String typeName, 
                  Filter filter, String propertySetName) {
        super(propertyNames, handle, version, typeName, filter);
        setPropertySetName(propertySetName);
    }

    /**
     * returns the name of the property set targeted by a query
     */
    public String getPropertySetName() {
        return propertySetName;
    }

    /**
     * @see CASQuery_Impl#getPropertySetName()
     */
    public void setPropertySetName(String propertySetName) {
        this.propertySetName = propertySetName;
    }

    /**
     *
     *
     * @return 
     */
    public String toString() {
        String ret = this.getClass().getName() + ":\n";
        ret += super.toString();
        ret += ("propertySetName: " + propertySetName + "\n");
        return ret;
    }
}