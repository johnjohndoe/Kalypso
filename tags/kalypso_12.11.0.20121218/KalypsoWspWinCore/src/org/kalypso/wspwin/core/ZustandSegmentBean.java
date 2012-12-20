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
package org.kalypso.wspwin.core;

import java.math.BigDecimal;
import java.util.Locale;

/**
 * Represents a line from the lower part of a .str file.
 * 
 * @author Belger
 */
public class ZustandSegmentBean
{
  private final BigDecimal m_stationFrom;

  private final BigDecimal m_stationTo;
  private final String m_fileNameFrom;
  private final String m_fileNameTo;
  private final double m_distanceVL;
  private final double m_distanceHF;
  private final double m_distanceVR;

  public ZustandSegmentBean( final BigDecimal stationFrom, final BigDecimal stationTo, final String fileNameFrom, final String fileNameTo, final double distanceVL, final double distanceHF, final double distanceVR )
  {
    m_stationFrom = stationFrom;
    m_stationTo = stationTo;
    m_fileNameFrom = fileNameFrom;
    m_fileNameTo = fileNameTo;
    m_distanceVL = distanceVL;
    m_distanceHF = distanceHF;
    m_distanceVR = distanceVR;
  }

  public double getDistanceHF( )
  {
    return m_distanceHF;
  }

  public double getDistanceVL( )
  {
    return m_distanceVL;
  }

  public double getDistanceVR( )
  {
    return m_distanceVR;
  }

  public String getFileNameFrom( )
  {
    return m_fileNameFrom;
  }

  public String getFileNameTo( )
  {
    return m_fileNameTo;
  }

  public BigDecimal getStationFrom( )
  {
    return m_stationFrom;
  }

  public BigDecimal getStationTo( )
  {
    return m_stationTo;
  }

  public String formatLine( )
  {
    return String.format( Locale.US, "%.6f %.6f %.4f %.4f %.4f %s %s", m_stationFrom, m_stationTo, m_distanceVL, m_distanceHF, m_distanceVR, m_fileNameFrom, m_fileNameTo ); //$NON-NLS-1$
  }
}