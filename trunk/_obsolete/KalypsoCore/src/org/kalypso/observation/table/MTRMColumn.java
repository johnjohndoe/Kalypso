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
package org.kalypso.observation.table;

import org.kalypso.commons.tuple.impl.SimpleColumnKey;
import org.kalypso.observation.result.IComponent;

/**
 * @author schlienger
 */
public abstract class MTRMColumn extends SimpleColumnKey
{
  private final IComponent m_keyComponent;
  private final int m_position;

  public MTRMColumn( final int position, final String name, final IComponent keyComponent, final Class<?> valueClass )
  {
    super( name, valueClass );
    m_position = position;
    
    m_keyComponent = keyComponent;
  }
  
  public int getPosition( )
  {
    return m_position;
  }

  public IComponent getKeyComponent( )
  {
    return m_keyComponent;
  }
  
  /**
   * Columns/Components are said to be compatible when their valueTypeNames are equals.
   * 
   * @return true if both components are compatible.
   */
  public boolean isCompatible( final IComponent c )
  {
    return m_keyComponent.getValueTypeName().equals( c.getValueTypeName() );
  }
}
