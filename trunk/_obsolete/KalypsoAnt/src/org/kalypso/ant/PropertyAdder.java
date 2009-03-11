/*--------------- Kalypso-Header ------------------------------------------

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

--------------------------------------------------------------------------*/

package org.kalypso.ant;

import java.util.HashMap;
import java.util.Map;

import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;

/**
 * Helper class to add properties within an ant task.
 *
 * @author belger
 */
public class PropertyAdder
{
  private final Map<String, String> m_addedAttributes = new HashMap<String, String>();
  private final Task m_task;

  public PropertyAdder( final Task task )
  {
    m_task = task;
  }

  /**
   * Actually add the given property/value to the project after writing a log message.
   * 
   * @note: Taken from {@link XmlProperty}in Ant.
   */
  public void addProperty( final String name, String value, final String id )
  {
    String msg = name + ":" + value;
    if( id != null )
      msg += ( "(id=" + id + ")" );

    final Project project = m_task.getProject();
    if(project!=null)
      project.log( msg, Project.MSG_DEBUG );

    if( m_addedAttributes.containsKey( name ) )
    {
      // If this attribute was added by this task, then
      // we append this value to the existing value.
      // We use the setProperty method which will
      // forcibly override the property if it already exists.
      // We need to put these properties into the project
      // when we read them, though (instead of keeping them
      // outside of the project and batch adding them at the end)
      // to allow other properties to reference them.
      value = m_addedAttributes.get( name ) + "," + value;
      project.setProperty( name, value );
    }
    else
      project.setNewProperty( name, value );

    m_addedAttributes.put( name, value );

    if( id != null )
      project.addReference( id, value );
  }

}
