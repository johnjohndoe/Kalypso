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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;

/**
 * @author Holger Albert
 */
public class CodedTrippleProfile
{
  private static final String UNKNOWN_POINTS_HORIZON_ID = "unknownPointsHorizon";

  private final String m_name;

  private final List<CodedTrippleProfilePoint> m_points;

  private final CodedTrippleHorizonMapper m_mapper;

  public CodedTrippleProfile( String name )
  {
    m_name = name;
    m_points = new ArrayList<>();
    m_mapper = new CodedTrippleHorizonMapper();
  }

  public void addProfilePoint( CodedTrippleProfilePoint point )
  {
    /* Only store points in correct order. */
    m_points.add( point );
  }

  public String getName( )
  {
    return m_name;
  }

  /**
   * This function returns all profile points of every horizon.
   * 
   * @return All profile points of every horizon.
   */
  public CodedTrippleProfilePoint[] getProfilePoints( )
  {
    return m_points.toArray( new CodedTrippleProfilePoint[] {} );
  }

  public CodedTrippleProfileHorizon[] getProfileHorizons( )
  {
    /* Memory for the horizons. */
    Map<String, CodedTrippleProfileHorizon> horizons = new LinkedHashMap<>();

    /* Loop all points. */
    for( CodedTrippleProfilePoint point : m_points )
    {
      /* Get the code of the point. */
      String code = point.getCode();

      /* Get the horizon id for that code. */
      String horizonId = m_mapper.getHorizonId( code );
      if( StringUtils.isEmpty( horizonId ) )
        horizonId = UNKNOWN_POINTS_HORIZON_ID;

      /* If there is no coded tripple profile horizon for that horizon id, create a new one. */
      if( !horizons.containsKey( horizonId ) )
        horizons.put( horizonId, new CodedTrippleProfileHorizon( horizonId ) );

      /* Add the profile point to the correct horizon. */
      CodedTrippleProfileHorizon horizon = horizons.get( horizonId );
      horizon.addProfilePoint( point );
    }

    return horizons.values().toArray( new CodedTrippleProfileHorizon[] {} );
  }

  public BigDecimal getStation( )
  {
    // TODO
    return null;
  }
}