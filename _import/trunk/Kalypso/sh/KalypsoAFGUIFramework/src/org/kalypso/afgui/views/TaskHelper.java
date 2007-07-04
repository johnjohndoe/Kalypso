/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.afgui.views;

import java.util.List;

import org.eclipse.jface.viewers.TreePath;

import de.renew.workflow.base.Task;
import de.renew.workflow.base.TaskGroup;
import de.renew.workflow.base.Workflow;

/**
 * @author Stefan Kurzbach
 * 
 */
public class TaskHelper
{

  public static TreePath findPart( final String uri, final Workflow workflow )
  {
    return findPartInTaskGroups( uri, workflow.getTasks(), TreePath.EMPTY );
  }

  public static TreePath findPartInTaskGroups( final String uri, final List< ? extends Task> taskOrTaskGroups, final TreePath prefix )
  {
    TreePath result = null;
    for( final Task taskOrTaskGroup : taskOrTaskGroups )
    {
      if( taskOrTaskGroup.getURI().equals( uri ) )
      {
        return prefix.createChildPath( taskOrTaskGroup );
      }
      else if( taskOrTaskGroup instanceof TaskGroup )
      {
        result = findPartInTaskGroups( uri, ((TaskGroup) taskOrTaskGroup).getTasks(), prefix.createChildPath( taskOrTaskGroup ) );
      }
      if( result != null )
      {
        return result;
      }
    }
    return result;
  }
}
