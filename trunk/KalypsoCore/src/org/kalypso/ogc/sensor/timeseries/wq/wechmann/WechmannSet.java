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

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.java.util.DateUtilities;

/**
 * A Set of WechmannParams ordered by WGR and valid for the given Date.
 * 
 * @author schlienger
 */
public class WechmannSet
{
  private final SortedMap m_mapW;

  private final SortedMap m_mapQ;

  private final Date m_validity;

  /**
   * Constructor with Params only. Takes the minimum Date as delivered by
   * DateUtilities as validity.
   * 
   * @param wps
   */
  public WechmannSet( final WechmannParams[] wps )
  {
    this( DateUtilities.getMinimum(), wps );
  }

  /**
   * Sets the WechmannParams, they will be sorted by WGR (ascending). The order
   * is taken into account by the iterator when you call
   * <code>WechmannSet.iterator()</code>.
   * 
   * @param validity
   * @param wps
   */
  public WechmannSet( final Date validity, final WechmannParams[] wps )
  {
    m_validity = validity;

    m_mapW = new TreeMap();
    m_mapQ = new TreeMap();

    for( int i = 0; i < wps.length; i++ )
    {
      m_mapW.put( new Double( wps[i].getWGR() ), wps[i] );
      m_mapQ.put( new Double( wps[i].getQ4WGR() ), wps[i] );
    }
  }

  /**
   * Returns an iterator over the WechmannParamList backed by this WechmannSet.
   * The list ist sorted according to the WechmannParams' WGR attribute.
   * 
   * @return iterator over WechmannParams objects
   */
  public Iterator iterator()
  {
    return m_mapW.values().iterator();
  }

  /**
   * @return the validity of this set of WechmannParams.
   */
  public Date getValidity()
  {
    return m_validity;
  }

  /**
   * @param W
   * @return the WechmannParams that are relevant for the given Waterlevel.
   */
  public WechmannParams getForW(final double W)
  {
    final Double[] ds = (Double[]) m_mapW.keySet().toArray( new Double[0] );
    int i = Arrays.binarySearch( ArrayUtils.toPrimitive( ds ), W );

    if( i < 0 )
      i = -i - 1;

    return (WechmannParams) m_mapW.get( ds[i] );
  }

  /**
   * @param Q
   * @return the WechmannParams that are relevant for the given Runoff.
   */
  public WechmannParams getForQ(final double Q)
  {
    final Double[] ds = (Double[]) m_mapQ.keySet().toArray( new Double[0] );
    int i = Arrays.binarySearch( ArrayUtils.toPrimitive( ds ), Q );

    if( i < 0 )
      i = -i - 1;

    if( i >= ds.length )
      i = ds.length - 1;
    
    return (WechmannParams) m_mapQ.get( ds[i] );
  }
}