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
package org.kalypso.observation.result;

import javax.xml.namespace.QName;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

/**
 * @author schlienger
 */
public class Component implements IComponent
{
  private final String m_name;

  private final String m_description;

  private final String m_unit;

  private final QName m_valueTypeName;

  private final Object m_defaultValue;

  private final String m_frame;

  private final String m_id;

  public Component( final String id, final String name, final String description, final String unit, final String frame, final QName valueTypeName, final Object defaultValue )
  {
    m_id = id;
    m_unit = unit;
    m_frame = frame;
    if( name == null )
    {
      throw new IllegalArgumentException( "name argument must not be null for " + getClass().getName() );
    }
    if( valueTypeName == null )
    {
      throw new IllegalArgumentException( "valueTypeName argument must not be null for " + getClass().getName() );
    }

    m_name = name;
    m_description = description;
    m_valueTypeName = valueTypeName;
    m_defaultValue = defaultValue;
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object obj )
  {
    if( !(obj instanceof IComponent) )
    {
      return false;
    }

    if( this == obj )
    {
      return true;
    }

    // subclass are sure that the component they receive is of the same class
    if( !getClass().getName().equals( obj.getClass().getName() ) )
    {
      return false;
    }

    final IComponent comp = (IComponent) obj;

    final EqualsBuilder builder = new EqualsBuilder();

    fillEqualsBilder( comp, builder );

    return builder.isEquals();
  }

  protected void fillEqualsBilder( final IComponent comp, final EqualsBuilder builder )
  {
    builder.append( comp.getDefaultValue(), m_defaultValue );
    builder.append( comp.getDescription(), m_description );
    builder.append( comp.getName(), m_name );
    builder.append( comp.getValueTypeName(), m_valueTypeName );
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode( )
  {
    final HashCodeBuilder builder = new HashCodeBuilder();

    fillHashCodeBuilder( builder );

    return builder.toHashCode();
  }

  protected void fillHashCodeBuilder( final HashCodeBuilder builder )
  {
    builder.append( m_defaultValue );
    builder.append( m_description );
    builder.append( m_name );
    builder.append( m_valueTypeName );
  }

  /**
   * @see org.kalypso.observation.result.IComponent#getId()
   */
  public String getId( )
  {
    return m_id;
  }

  /**
   * @see org.kalypso.om.tuple.IComponent#getName()
   */
  public String getName( )
  {
    return m_name;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return m_name + "[" + m_valueTypeName + "]";
  }

  /**
   * @see org.kalypso.observation.result.IComponent#getValueTypeName()
   */
  public QName getValueTypeName( )
  {
    return m_valueTypeName;
  }

  /**
   * @see org.kalypso.om.tuple.IComponent#getDefaultValue()
   */
  public Object getDefaultValue( )
  {
    return m_defaultValue;
  }

  /**
   * @see org.kalypso.observation.result.IComponent#getDescription()
   */
  public String getDescription( )
  {
    return m_description;
  }

  public String getUnit( )
  {
    return m_unit;
  }

  /**
   * @see org.kalypso.observation.result.IComponent#getFrame()
   */
  public String getFrame( )
  {
    return m_frame;
  }

  /**
   * @see org.kalypso.observation.result.IComponent#compare(java.lang.Object, java.lang.Object)
   */
  public int compare( final Object objFirst, final Object objSecond )
  {
    // TODO perhaps someday implement something meaningful
    return ("" + objFirst).compareTo( "" + objSecond );
  }
}
