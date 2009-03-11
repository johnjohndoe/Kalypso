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

import java.util.Comparator;

/**
 * Compares two TupleResultColumn. They are equal if they have the same:
 * <ul>
 * <li>TupleResult
 * <li>KeyComponent
 * <li>ValueComponent
 * </ul>
 * 
 * @author schlienger
 */
public class TupleResultColumnComparator implements Comparator<TupleResultColumn>
{
  private static TupleResultColumnComparator m_instance = null;

  /**
   * @see java.util.Comparator#compare(T, T)
   */
  public int compare( TupleResultColumn o1, TupleResultColumn o2 )
  {
    if( o1.getTupleResult() == o2.getTupleResult() && o1.getKeyComponent() == o2.getKeyComponent() && o1.getValueComponent() == o2.getValueComponent() )
      return 0;

    return 1;
  }

  public static TupleResultColumnComparator getInstance( )
  {
    if( m_instance == null )
      m_instance = new TupleResultColumnComparator();
    
    return m_instance;
  }
}
