/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

/**
 * @author schlienger
 */
public class Component implements IComponent
{
  private final String m_name;
  private final String m_description;

  private final Class< ? > m_valueClass;

  private final Object m_defaultValue;

  public Component( String name, String description, Class< ? > valueClass )
  {
    this( name, description, valueClass, null );
  }

  public Component( String name, String description, Class< ? > valueClass, Object defaultValue )
  {
    m_name = name;
    m_description = description;
    m_valueClass = valueClass;
    m_defaultValue = defaultValue;
  }

  /**
   * @see org.kalypso.om.tuple.IComponent#getName()
   */
  public String getName( )
  {
    return m_name;
  }

  /**
   * @see org.kalypso.tuple.IColumnKey#getValueClass()
   */
  public Class< ? > getValueClass( )
  {
    return m_valueClass;
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
}
