/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.contribs.java.util;

import java.util.Date;
import java.util.Iterator;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author doemming
 */
public class ValueIterator implements Iterator
{

  private final Object m_start;

  private final int m_rows;

  private int m_pos = 0;

  private final Object m_end;

  public ValueIterator( Object start, Object end, int rows )
  {
    m_start = start;
    m_end = end;
    m_rows = rows;
  }

  /**
   * @see java.util.Iterator#remove()
   */
  public void remove()
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.Iterator#hasNext()
   */
  public boolean hasNext()
  {
    return m_pos < m_rows;
  }

  /**
   * @see java.util.Iterator#next()
   */
  public Object next()
  {
    int n = m_pos;
    m_pos++;
    if( m_start instanceof Double )
      return new Double( ( (Double)m_start ).doubleValue() + n
          * ( ( (Double)m_end ).doubleValue() - ( (Double)m_start ).doubleValue() ) / m_rows );
    if( m_start instanceof Integer )
      return new Integer( ( (Integer)m_start ).intValue() + n
          * ( ( (Integer)m_end ).intValue() - ( (Integer)m_start ).intValue() ) / m_rows );
    if( m_start instanceof Date )
      return new Date( ( (Date)m_start ).getTime() + n * ( ( (Date)m_end ).getTime() - ( (Date)m_start ).getTime() )
          / m_rows );

    return new UnsupportedOperationException();
  }
}
