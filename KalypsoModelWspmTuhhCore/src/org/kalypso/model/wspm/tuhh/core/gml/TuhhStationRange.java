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
package org.kalypso.model.wspm.tuhh.core.gml;

import java.math.BigDecimal;

/**
 * @author Gernot Belger
 */
public class TuhhStationRange
{
  private final BigDecimal m_from;

  private final BigDecimal m_to;

  private final boolean m_directionUpstreams;

  public TuhhStationRange( final TuhhCalculation calculation )
  {
    m_directionUpstreams = calculation.getReach().getWaterBody().isDirectionUpstreams();

    final BigDecimal startStation = calculation.getStartStation();
    final BigDecimal endStation = calculation.getEndStation();

    // REMARK: We sort from/to always in ascending order, regardless of the chosen direction of the river.
    // This is a precondition of the calculation core.
    if( startStation.compareTo( endStation ) < 0 )
    {
      m_from = startStation;
      m_to = endStation;
    }
    else
    {
      m_from = endStation;
      m_to = startStation;
    }
  }

  /**
   * Returns true if the given station is outside this range; i.e. if station < from or station > to.
   */
  public boolean isOutside( final BigDecimal station )
  {
    return m_from.compareTo( station ) > 0 || station.compareTo( m_to ) > 0;
  }

  public boolean getDirection( )
  {
    return m_directionUpstreams;
  }

  public double getExportFrom( )
  {
    return m_directionUpstreams ? m_from.doubleValue() : -m_to.doubleValue();
  }

  public double getExportTo( )
  {
    return m_directionUpstreams ? m_to.doubleValue() : -m_from.doubleValue();
  }

  public int getExportSign( )
  {
    if( m_directionUpstreams )
      return 1;
    else
      return -1;
  }
}