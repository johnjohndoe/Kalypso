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
/*
 * RPCStruct.java
 *
 * Created on 24. Oktober 2003, 17:03
 */
package org.deegree.enterprise.control;

/**
 * encapsulates a RPC struct.
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public interface RPCStruct {
  
    /**
     * returns the members of the struct
     *
     * @return members of the struct
     */
    RPCMember[] getMembers();

    /**
     * returns a named member of the struct. if no member with the passed name
     * is contained within the struct <tt>null</tt> will be returned.
     *
     * @param name name of the struct member
     *
     * @return struct member
     */
    RPCMember getMember( String name );

    /**
     * adds a new member to the struct
     *
     * @param member 
     */
    void addMember( RPCMember member );

    /**
     * removes a member identified by its name from the struct
     *
     * @param name 
     *
     * @return 
     */
    RPCMember removeMember( String name );
}