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

import org.deegree.enterprise.control.RPCMethodResponse;
import org.deegree.enterprise.control.RPCParameter;
import org.deegree.enterprise.control.RPCFault;

/**
 * The class encapsulates the result to a RPC. This can be an object or an
 * instance of <tt>RPCFault</tt> if an exception occured while performing a
 * RPC
 * hilmel@web.de mela
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public class RPCMethodResponse_Impl implements RPCMethodResponse {
    
    private boolean fault_ = false; 
    private RPCParameter[] return_ = null;
    private RPCFault fault = null;
    
    RPCMethodResponse_Impl(RPCParameter[] return_) {
        this.return_ = return_;
    }
    
    RPCMethodResponse_Impl(RPCFault fault) {
        this.fault = fault;
        fault_ = true;
    }
    
    /**
     * returns true if the result contains a fault and not the expected data
     *
     * @return true if a fault occured
     */
    public boolean hasFault() {
        return fault_;
    }

    /**
     * returns the result of a method call as array of <tt>RPCParameter</tt>s
     *
     * @return result parameters
     */
    public RPCParameter[] getReturn() {
        return return_;
    }

    /**
     * returns the fault object if a fault occured while performing a RPC
     *
     * @return fault object
     */
    public RPCFault getFault() {
        return fault;
    }
}