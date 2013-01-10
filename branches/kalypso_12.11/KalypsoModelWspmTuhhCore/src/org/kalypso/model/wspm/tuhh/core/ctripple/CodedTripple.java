/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Bj�rnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universit�t Hamburg-Harburg, Institut f�r Wasserbau, Hamburg, Germany
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

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;

/**
 * @author Holger Albert
 */
public class CodedTripple
{
  final Map<String, CodedTrippleProfile> m_profiles;

  private final CodedTrippleHorizonMapper m_mapper;

  final Set<String> m_badCodes;

  public CodedTripple( )
  {
    m_profiles = new LinkedHashMap<>();
    m_mapper = new CodedTrippleHorizonMapper();
    m_badCodes = new HashSet<>();
  }

  public void addProfilePoint( CodedTrippleProfilePoint point )
  {
    /* Get the code of the point. */
    String code = point.getCode();

    /* Get the horizon id for that code. */
    String horizonId = m_mapper.getHorizonId( code );
    if( !StringUtils.isEmpty( horizonId ) )
    {
      /* Id of of our profile parts. */
      String partId = m_mapper.getPartId( horizonId );

      /* There is no part id mapped for this horizon id. */
      if( StringUtils.isEmpty( partId ) )
        m_badCodes.add( code );
    }
    else
    {
      /* There is no horizon id mapped for this code. */
      m_badCodes.add( code );
    }

    /* Add the profile point to the correct profile. */
    String name = point.getName();
    if( !m_profiles.containsKey( name ) )
      m_profiles.put( name, new CodedTrippleProfile( name ) );

    CodedTrippleProfile profile = m_profiles.get( name );
    profile.addProfilePoint( point );
  }

  public CodedTrippleProfile[] getProfiles( )
  {
    return m_profiles.values().toArray( new CodedTrippleProfile[] {} );
  }
}