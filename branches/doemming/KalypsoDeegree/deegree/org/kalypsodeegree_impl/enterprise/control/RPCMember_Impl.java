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
package org.deegree_impl.enterprise.control;

import org.deegree.enterprise.control.RPCMember;

/**
 * Extends <tt>RPCParameter_Impl</tt> by adding a new member variable 'name'.
 * A <tt>RPCMember</tt> is contained within a <tt>RPCStruct</tt> that enables
 * the creation of complex parameters.
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public class RPCMember_Impl extends RPCParameter_Impl implements RPCMember {
    
    private String name = null;
    
    /**
     * @param type class of the parameter
     * @param value the value of the parameter
     * @param name name of the member
     */
    public RPCMember_Impl(Class type, Object value, String name) {
        super( type, value );
        this.name = name;
    }
    
    /**
     * returns the name of the struct member
     *
     * @return name of the member
     */
    public String getName() {
        return name;
    }
    
}