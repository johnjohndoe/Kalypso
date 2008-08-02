/*--------------- Kalypso-Header --------------------------------------------------------------------

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
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.timeseries.wq.wechmann;

import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.SortedMap;
import java.util.TreeMap;

import org.kalypso.core.i18n.Messages;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.wq.IWQConverter;
import org.kalypso.ogc.sensor.timeseries.wq.WQException;

/**
 * A List of WechmannSets sorted according to the date of validity of the WechmannSet objects. You can call the iterator
 * to step through the list in the ascending order of date of validity.
 * 
 * @author schlienger
 */
public class WechmannGroup implements IWQConverter
{
  private final SortedMap<Date, WechmannSet> m_map = new TreeMap<Date, WechmannSet>();

  /**
   * @param wsets
   */
  public WechmannGroup( final WechmannSet[] wsets )
  {
    for( int i = 0; i < wsets.length; i++ )
      m_map.put( wsets[i].getValidity(), wsets[i] );
  }

  /**
   * @return Iterator on the WechmannSet objects
   */
  public Iterator<WechmannSet> iterator( )
  {
    return m_map.values().iterator();
  }

  /**
   * Returns the WechmannSet that is valid for the given date.
   * 
   * @throws WQException
   */
  public WechmannSet getFor( final Date d ) throws WQException
  {
    final Date[] dates = m_map.keySet().toArray( new Date[0] );
    int i = Arrays.binarySearch( dates, d );

    if( i < 0 )
      i = -i - 2;

    if( i < 0 )
      throw new WQException( Messages.getString("org.kalypso.ogc.sensor.timeseries.wq.wechmann.WechmannGroup.0") ); //$NON-NLS-1$

    return m_map.get( dates[i] );
  }

  /**
   * @see org.kalypso.ogc.sensor.timeseries.wq.IWQConverter#computeW(java.util.Date, double)
   */
  public double computeW( Date date, double Q ) throws WQException
  {
    final WechmannParams params = getFor( date ).getForQ( Q );
    return WechmannFunction.computeW( params, Q );
  }

  /**
   * Returns 0.0, if W is too big for current validity
   * @see org.kalypso.ogc.sensor.timeseries.wq.IWQConverter#computeQ(java.util.Date, double)
   */
  public double computeQ( Date date, double W ) throws WQException
  {
    final WechmannParams params = getFor( date ).getForW( W );
    if( params == null )
      return 0.0;
    
    return WechmannFunction.computeQ( params, W );
  }

  /**
   * @see org.kalypso.ogc.sensor.timeseries.wq.IWQConverter#getFromType()
   */
  public String getFromType()
  {
    // HARDCODED: Wechman always converts from W to Q?
    return TimeserieConstants.TYPE_WATERLEVEL;
  }

  /**
   * @see org.kalypso.ogc.sensor.timeseries.wq.IWQConverter#getToType()
   */
  public String getToType()
  {
    // HARDCODED: Wechman always converts from W to Q?
    return TimeserieConstants.TYPE_RUNOFF;
  }
}