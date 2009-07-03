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
package org.kalypso.dwd;

import java.util.Date;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * 
 * this represents a observation that is read from DWD-rasterformat (LM or LM2)
 * 
 * @author doemming
 */
public class DWDObservationRaster
{

  private final int m_dwdKey;

  private final SortedMap m_valueHash = new TreeMap(); // (date,double[])

  private final int m_maxCells;

  /**
   * @param dwdKey
   *          type of observation
   *  
   */
  public DWDObservationRaster( int dwdKey, int maxCells )
  {
    m_dwdKey = dwdKey;
    m_maxCells = maxCells;
  }

  /**
   * 
   * @return type of observation
   */
  public int getDwdKey()
  {
    return m_dwdKey;
  }

  public void setValueFor( final Date date, final int cellPos, final double value )
  {
    if( !m_valueHash.containsKey( date ) )
    {
      m_valueHash.put( date, new double[m_maxCells] );
    }
    final double[] values = (double[])m_valueHash.get( date );
    values[cellPos] = value;
  }

  public double getValueFor( Date date, int cellPos )
  {
    final double[] values = (double[])m_valueHash.get( date );
    return values[cellPos];
  }

  public Date[] getDates()
  {
    final SortedSet sort = new TreeSet( m_valueHash.keySet() );
    return (Date[])sort.toArray( new Date[sort.size()] );
  }

  public Date[] getDates( Date min, Date max )
  {
    final SortedSet sort = new TreeSet( m_valueHash.keySet() );
    if( min == null )
      min = (Date)sort.first();
    if( max == null )
      max = (Date)sort.last();
    final SortedSet result = sort.subSet( min, max );
    return (Date[])result.toArray( new Date[result.size()] );
  }
}
