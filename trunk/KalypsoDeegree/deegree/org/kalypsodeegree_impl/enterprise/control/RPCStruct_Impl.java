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

import java.util.HashMap;

import org.deegree.enterprise.control.RPCMember;
import org.deegree.enterprise.control.RPCStruct;

/**
 * The class encapsulates a RPC struct.
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class RPCStruct_Impl implements RPCStruct
{

  private HashMap members = null;

  public RPCStruct_Impl()
  {
    members = new HashMap();
  }

  public RPCStruct_Impl( RPCMember[] mem )
  {
    this();
    for( int i = 0; i < mem.length; i++ )
    {
      members.put( mem[i].getName(), mem[i] );
    }
  }

  /**
   * returns the members of the struct
   * 
   * @return members of the struct
   */
  public RPCMember[] getMembers()
  {
    RPCMember[] m = new RPCMember[members.size()];
    return (RPCMember[])members.values().toArray( m );
  }

  /**
   * returns a named member of the struct. if no member with the passed name is
   * contained within the struct <tt>null</tt> will be returned.
   * 
   * @param name
   *          name of the struct member
   * 
   * @return struct member
   */
  public RPCMember getMember( String name )
  {
    return (RPCMember)members.get( name );
  }

  /**
   * adds a new member to the struct
   * 
   * @param member
   */
  public void addMember( RPCMember member )
  {
    members.put( member.getName(), member );
  }

  /**
   * removes a member identified by its name from the struct
   * 
   * @param name
   * 
   * @return
   */
  public RPCMember removeMember( String name )
  {
    return (RPCMember)members.remove( name );
  }

}