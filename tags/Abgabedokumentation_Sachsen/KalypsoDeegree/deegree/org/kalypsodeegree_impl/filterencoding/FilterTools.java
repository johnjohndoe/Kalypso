/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 
 history:
  
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
     
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.filterencoding;

import java.util.ArrayList;
import java.util.Stack;

import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class FilterTools
{
  /**
   * Traverses the <tt>Filter</tt> -tree and returns the first BBOX-Operation
   * that is found and a <tt>Filter</tt> that is equal to the given one minus
   * the BBOX-Operation.
   * <p>
   * 
   * @param filter
   *          search starts here
   * @return [0]: <tt>GM_Envelope</tt> (BBOX), [1]: <tt>Filter</tt>
   * @throws Exception
   */
  public static Object[] extractFirstBBOX( ComplexFilter filter ) throws Exception
  {
    Debug.debugMethodBegin();

    // [0]: GM_Envelope, [1]: Filter
    Object[] objects = new Object[2];
    objects[1] = filter;

    // sanity check (Filter empty)
    if( filter == null )
    {
      return objects;
    }

    // used as LIFO-queue
    Stack operations = new Stack();
    operations.push( filter.getOperation() );

    while( !operations.isEmpty() )
    {
      // get the first element of the queue
      Operation operation = (Operation)operations.pop();

      switch( operation.getOperatorId() )
      {
      case OperationDefines.BBOX:
      {
        // found BBOX
        objects[0] = ( (SpatialOperation)operation ).getBoundingBox();
        break;
      }
      case OperationDefines.AND:
      {
        ArrayList arguments = ( (LogicalOperation)operation ).getArguments();

        for( int i = 0; i < arguments.size(); i++ )
        {
          operations.push( arguments.get( i ) );

          //                        if ( ( (Operation)arguments.get( i ) ).getOperatorId() ==
          // OperationDefines.BBOX ) {
          //                            // remove the BBOX-Operation from the AND-tree
          //                            System.out.println( "Removing: " + i );
          //                            arguments.remove( i );
          //                            break;
          //                        }
        }

        break;
      }
      }

      // BBOX found?
      if( objects[0] != null )
      {
        break;
      }
    }

    // special case: Filter contains only the BBOX-Operation
    if( filter.getOperation().getOperatorId() == OperationDefines.BBOX )
    {
      //objects[1] = null;
    }

    Debug.debugMethodEnd();
    return objects;
  }
}