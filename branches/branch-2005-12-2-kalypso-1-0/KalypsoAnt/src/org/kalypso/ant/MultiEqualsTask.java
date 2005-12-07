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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.taskdefs.XmlProperty;

/**
 * This task allows to set mutliple properties depending on the result of comparing two strings.
 * 
 * @author belger
 */
public class MultiEqualsTask extends Task
{
  public final static class PropertyToSet
  {
    private String m_name;

    private String m_valueThen;

    /** Value of the property to set, if the compared string are not equal */
    private String m_valueElse;

    public void setName( String name )
    {
      m_name = name;
    }

    /** Value of the property to set, if the compared strings are equal */
    public void setValueElse( String valueElse )
    {
      m_valueElse = valueElse;
    }

    /** Name of the property to set */
    public void setValueThen( String valueThen )
    {
      m_valueThen = valueThen;
    }
    
    public String getName()
    {
      return m_name;
    }
    public String getValueElse()
    {
      return m_valueElse;
    }
    public String getValueThen()
    {
      return m_valueThen;
    }
  }

  private String m_arg1;
  private String m_arg2;

  private List m_properties = new ArrayList( 5 );

  /** List of added ant-properties */
  private final Map m_addedAttributes = new HashMap();

  /** Adds a property to set */
  public void addConfiguredProperty( final PropertyToSet property )
  {
    m_properties.add( property );
  }

  /** first string to compare */
  public void setArg1( String arg1 )
  {
    m_arg1 = arg1;
  }

  /** Second string to compare */
  public void setArg2( String arg2 )
  {
    m_arg2 = arg2;
  }

  /**
   * @see org.apache.tools.ant.Task#execute()
   */
  public void execute() throws BuildException
  {
    if( m_arg1 == null )
      throw new BuildException( "arg1 was never set" );

    if( m_arg2 == null )
      throw new BuildException( "arg2 was never set" );

    final boolean isEquals = m_arg1.equals( m_arg2 );
    final String msg = isEquals ? "Strings are equal" : "Strings are not equal";
    log( msg, Project.MSG_DEBUG );

    for( final Iterator iter = m_properties.iterator(); iter.hasNext(); )
    {
      final PropertyToSet property = (PropertyToSet)iter.next();

      final String valueToSet = isEquals ? property.getValueThen() : property.getValueElse();
      addProperty( property.getName(), valueToSet, null );
    }
  }

  /**
   * Actually add the given property/value to the project after writing a log message.
   * 
   * @note: Taken from {@link XmlProperty}in Ant.
   */
  private void addProperty( String name, String value, String id )
  {
    String msg = name + ":" + value;
    if( id != null )
      msg += ( "(id=" + id + ")" );

    log( msg, Project.MSG_DEBUG );

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
      value = (String)m_addedAttributes.get( name ) + "," + value;
      getProject().setProperty( name, value );
    }
    else
      getProject().setNewProperty( name, value );

    m_addedAttributes.put( name, value );

    if( id != null )
      getProject().addReference( id, value );
  }

}
