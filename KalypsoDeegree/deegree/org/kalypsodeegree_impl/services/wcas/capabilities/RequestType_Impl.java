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
package org.deegree_impl.services.wcas.capabilities;

import java.util.ArrayList;

import org.deegree.services.capabilities.DCPType;
import org.deegree.services.wcas.capabilities.RequestType;

/**
 *
 * <p>-----------------------------------------------------</p>
 *
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer</a>
 * @version $Revision$ $Date$
 */
public class RequestType_Impl implements RequestType {
    
    private ArrayList dcpTypes = null;
    
    RequestType_Impl(DCPType[] dcpTypes) {
        this.dcpTypes = new ArrayList();
        setDCPTypes( dcpTypes );
    }
    
    /**
     * The only available distributed computing platform is HTTP
     * for which two request methods are defined; GET and POST.
     * The onlineResource attribute indicates the URL prefix for
     * HTTP GET requests (everything before the question mark and
     * query string:http://hostname[:port]/path/scriptname); for HTTP POST
     * requests, onlineResource is the complete URL.
     */
    public DCPType[] getDCPTypes() {
        return (DCPType[])dcpTypes.toArray( new DCPType[dcpTypes.size()] );
    }
    
    /**
     * @see RequestType_Impl#getDCPTypes()
     */
    public void setDCPTypes(DCPType [] dcpTypes) {
        this.dcpTypes.clear();
        if ( dcpTypes != null ) {
            for (int i = 0; i < dcpTypes.length; i++) {
                addDCPType( dcpTypes[i] );
            }
        }
    }
    
    /**
     * @see RequestType_Impl#getDCPTypes()
     */
    public void addDCPType(DCPType dcpType) {
        dcpTypes.add( dcpType );
    }
}
