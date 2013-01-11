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
  private final Map<String, CodedTrippleProfile> m_profiles;

  private final CodedTrippleHorizonMapper m_mapper;

  private final Set<String> m_badCodes;

  public CodedTripple( final CodedTrippleHorizonMapper mapper )
  {
    m_profiles = new LinkedHashMap<>();
    m_mapper = mapper;
    m_badCodes = new HashSet<>();
  }

  public void addProfilePoint( final CodedTrippleProfilePoint point )
  {
    /* Get the code of the point. */
    final String code = point.getCode();

    /* Get the horizon id for that code. */
    final String horizonId = m_mapper.getHorizonId( code );
    if( !StringUtils.isBlank( horizonId ) )
    {
      /* Id of of our profile parts. */
      final String partId = m_mapper.getPartId( horizonId );

      /* There is no part id mapped for this horizon id. */
      if( partId == null )
        m_badCodes.add( code );
    }
    else
    {
      /* There is no horizon id mapped for this code. */
      m_badCodes.add( code );
    }

    /* Add the profile point to the correct profile. */
    final String name = point.getName();
    if( !m_profiles.containsKey( name ) )
      m_profiles.put( name, new CodedTrippleProfile( name, m_mapper ) );

    final CodedTrippleProfile profile = m_profiles.get( name );
    profile.addProfilePoint( point );
  }

  public CodedTrippleProfile[] getProfiles( )
  {
    return m_profiles.values().toArray( new CodedTrippleProfile[] {} );
  }

  public CodedTrippleHorizonMapper getMapper( )
  {
    return m_mapper;
  }

  public String[] getBadCodes( )
  {
    return m_badCodes.toArray( new String[] {} );
  }
}