/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.core.ctripple;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Holger Albert
 */
public class CodedTrippleProfile
{
  private final BigDecimal m_station;

  private final List<CodedTrippleProfilePoint> m_points;

  public CodedTrippleProfile( BigDecimal station )
  {
    m_station = station;
    m_points = new ArrayList<>();
  }

  public void addProfilePoint( CodedTrippleProfilePoint point )
  {
    /* Only store points in correct order. */
    m_points.add( point );
  }

  public BigDecimal getStation( )
  {
    return m_station;
  }

  public CodedTrippleProfileHorizon[] getProfileHorizons( )
  {
    // String code = point.getCode();

    /* horizon from profiles. */
    // String horizonId = horizonMapper.getHorizonId( code );

    /* Id of of our profile objects. */
    // horizonMapper.getPartId(horizontId);

    // TODO Create horizons here... because codes may have changed...
    // TODO hash by horizonId...
    return null;
  }
}