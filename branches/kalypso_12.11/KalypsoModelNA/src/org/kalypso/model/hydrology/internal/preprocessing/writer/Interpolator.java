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
package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.util.Date;
import java.util.SortedMap;

/**
 * Simple linear interpolator; receives Date/Double pairs, returns simple linear interpolation from the selected Date
 * segment
 * 
 * @author antanas
 */
class Interpolator
{
  private final SortedMap<Date, Double> m_valuesMap;

  public Interpolator( final SortedMap<Date, Double> valuesMap )
  {
    m_valuesMap = valuesMap;
  }

  public double getValue( final Date position )
  {
    if( m_valuesMap.isEmpty() )
      return Double.NaN;
    if( m_valuesMap.firstKey().after( position ) )
      return m_valuesMap.get( m_valuesMap.firstKey() );
    if( m_valuesMap.lastKey().before( position ) )
      return m_valuesMap.get( m_valuesMap.lastKey() );
    if( m_valuesMap.keySet().contains( position ) )
      return m_valuesMap.get( position );

    final Date lowerKey = m_valuesMap.headMap( position ).lastKey();
    final Date upperKey = m_valuesMap.tailMap( position ).firstKey();
    final double valueLower = m_valuesMap.get( lowerKey );
    final double valueUpper = m_valuesMap.get( upperKey );

    return valueLower + (position.getTime() - lowerKey.getTime()) * (valueUpper - valueLower) / (upperKey.getTime() - lowerKey.getTime());
  }
}
