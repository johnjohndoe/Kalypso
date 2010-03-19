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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * This represents a observation that is read from DWD-Rasterformat (LM or LM2).
 * 
 * @author doemming
 */
public class DWDObservationRaster
{
  /**
   * The value hash.
   */
  private final SortedMap<Date, List<Double>> m_valueHash = new TreeMap<Date, List<Double>>();

  /**
   * The type of the observation.
   */
  private final int m_dwdKey;

  /**
   * The unit of the values of this observation.
   */
  private final String m_unit;

  /**
   * The constructor.
   * 
   * @param dwdKey
   *          The type of the observation.
   * @param unit
   *          The unit of the values of this observation.
   */
  public DWDObservationRaster( final int dwdKey, final String unit )
  {
    m_dwdKey = dwdKey;
    m_unit = unit;
  }

  /**
   * This function returns the type of the observation.
   * 
   * @return The type of the observation.
   */
  public int getDwdKey( )
  {
    return m_dwdKey;
  }

  /**
   * This function returns the unit of the values of this observation.
   * 
   * @return The unit of the values of this observation.
   */
  public String getUnit( )
  {
    return m_unit;
  }

  public void setValueFor( final Date date, final int cellPos, final double value )
  {
    if( !m_valueHash.containsKey( date ) )
      m_valueHash.put( date, new ArrayList<Double>() );

    final List<Double> values = m_valueHash.get( date );
    values.add( new Double( value ) );
  }

  public double getValueFor( final Date date, final int cellPos )
  {
    final List<Double> values = m_valueHash.get( date );
    return values.get( cellPos ).doubleValue();
  }

  /** Returns all known dates by this raster. The dates are sorted in ascending order. */
  public Date[] getDates( )
  {
    return m_valueHash.keySet().toArray( new Date[m_valueHash.size()] );
  }

  public Date[] getDates( Date min, Date max )
  {
    if( min == null )
      min = m_valueHash.firstKey();
    if( max == null )
      max = m_valueHash.lastKey();
    final SortedMap<Date, List<Double>> result = m_valueHash.subMap( min, max );
    return result.keySet().toArray( new Date[result.size()] );
  }

  public Date getBaseDate( )
  {
    final Date[] dates = getDates();
    final Date firstRasterDate = dates == null || dates.length == 0 ? null : dates[0];
    if( firstRasterDate == null )
      return null;

    // REMARK: we assume that the first date is always BaseDate + 1
    final Calendar instance = Calendar.getInstance();
    instance.setTime( firstRasterDate );
    instance.add( Calendar.HOUR, -1 );

    return instance.getTime();
  }

  /**
   * This function returns the max number of cells of a raster (for a date).
   * 
   * @return The max number of cells.
   */
  public int getMaxCells( )
  {
    Date firstKey = m_valueHash.firstKey();
    if( firstKey == null )
      return 0;

    List<Double> firstValue = m_valueHash.get( firstKey );
    if( firstValue == null )
      return 0;

    return firstValue.size();
  }
}