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
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author Holger Albert
 */
public class CodedTripple
{
  final Map<BigDecimal, CodedTrippleProfile> m_profiles;

  final Set<String> m_badCodes;

  public CodedTripple( )
  {
    m_profiles = new LinkedHashMap<>();
    m_badCodes = new HashSet<>();
  }

  public void addProfilePoint( CodedTrippleProfilePoint point )
  {
    // Check for code...
    // String code = point.getCode();

    /* horizon from profiles. */
    // String horizonId = horizonMapper.getHorizonId( code );

    /* Id of of our profile objects. */
    // horizonMapper.getPartId(horizontId);

    // TODO Hash by name...
    BigDecimal station = point.getStation();
    if( !m_profiles.containsKey( station ) )
      m_profiles.put( station, new CodedTrippleProfile( station ) );

    CodedTrippleProfile profile = m_profiles.get( station );
    profile.addProfilePoint( point );
  }

  public CodedTrippleProfile[] getProfiles( )
  {
    return m_profiles.values().toArray( new CodedTrippleProfile[] {} );
  }
}