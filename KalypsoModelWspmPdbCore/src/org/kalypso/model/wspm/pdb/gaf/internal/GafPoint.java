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
package org.kalypso.model.wspm.pdb.gaf.internal;

import java.math.BigDecimal;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * represents one line of a gaf file
 * 
 * @author Gernot Belger
 */
public class GafPoint
{
  private final BigDecimal m_station;

  private final String m_pointId;

  private final BigDecimal m_width;

  private final BigDecimal m_height;

  private final String m_code;

  private final String m_roughnessClass;

  private final String m_vegetationClass;

  private final BigDecimal m_rw;

  private final BigDecimal m_hw;

  private final String m_hyk;

  public GafPoint( final BigDecimal station, final String pointId, final BigDecimal width, final BigDecimal height, final String code, final String roughnessClass, final String vegetationClass, final BigDecimal rw, final BigDecimal hw, final String hyk )
  {
    m_station = station;
    m_pointId = pointId;
    m_width = width;
    m_height = height;
    m_code = code;
    m_roughnessClass = roughnessClass;
    m_vegetationClass = vegetationClass;
    m_rw = rw;
    m_hw = hw;
    m_hyk = hyk;
  }

  public String getPointId( )
  {
    return m_pointId;
  }

  public BigDecimal getStation( )
  {
    return m_station;
  }

  public String getCode( )
  {
    return m_code;
  }

  public BigDecimal getWidth( )
  {
    return m_width;
  }

  public BigDecimal getHeight( )
  {
    return m_height;
  }

  public String getHyk( )
  {
    return m_hyk;
  }

  public Coordinate getCoordinate( )
  {
    if( m_rw == null || m_hw == null )
      return null;

    return new Coordinate( m_rw.doubleValue(), m_hw.doubleValue() );
  }
}