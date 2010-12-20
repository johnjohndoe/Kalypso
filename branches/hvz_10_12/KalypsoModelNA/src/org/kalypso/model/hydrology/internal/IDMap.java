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
package org.kalypso.model.hydrology.internal;

import java.util.Comparator;

class IDMap
{
  public static final Comparator< IDMap> COMPARATOR = new Comparator<IDMap>()
  {
    @Override
    public int compare( final IDMap m1, final IDMap m2 )
    {
      final int typeDiff = m1.getType() - m2.getType();
      if( typeDiff != 0 )
        return typeDiff;
      return m1.getAsciiID() - m2.getAsciiID();
    }
  };

  private final int m_type;

  private final int m_asciiID;

  public IDMap( final int asciiID, final int type )
  {
    this.m_type = type;
    this.m_asciiID = asciiID;
  }

  public int getAsciiID( )
  {
    return m_asciiID;
  }

  public int getType( )
  {
    return m_type;
  }

  @Override
  public String toString( )
  {
    return m_asciiID + " [" + m_type + "]"; //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object other )
  {
    if( !(other instanceof IDMap) )
      return false;
    final IDMap otherMap = (IDMap) other;
    return otherMap.getAsciiID() == m_asciiID && otherMap.getType() == m_type;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode( )
  {
    return m_asciiID + m_type * 100000;
  }
}