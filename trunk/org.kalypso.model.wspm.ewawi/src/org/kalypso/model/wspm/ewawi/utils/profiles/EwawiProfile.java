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
package org.kalypso.model.wspm.ewawi.utils.profiles;

import java.math.BigDecimal;
import java.util.SortedMap;
import java.util.TreeMap;

import org.kalypso.model.wspm.ewawi.data.EwawiProLine;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiHorizont;

/**
 * A EWAWI+ profile.
 * 
 * @author Holger Albert
 */
public class EwawiProfile
{
  private final BigDecimal m_station;

  private final SortedMap<Integer, EwawiProfilePart> m_parts;

  public EwawiProfile( final BigDecimal station )
  {
    m_station = station;
    m_parts = new TreeMap<>();
  }

  public BigDecimal getStation( )
  {
    return m_station;
  }

  public EwawiProfilePart[] getParts( )
  {
    return m_parts.values().toArray( new EwawiProfilePart[] {} );
  }

  public EwawiProfilePart getBasePart( )
  {
    final EwawiProfilePart[] parts = getParts();
    for( final EwawiProfilePart part : parts )
    {
      final Integer horizont = part.getHorizont();
      if( horizont.intValue() == EwawiHorizont._0.getKey() )
        return part;
    }

    return null;
  }

  public void addProLine( final EwawiProLine proLine )
  {
    final Integer horizont = proLine.getHorizont().getKey();
    if( !m_parts.containsKey( horizont ) )
      m_parts.put( horizont, new EwawiProfilePart( horizont ) );

    final EwawiProfilePart part = m_parts.get( horizont );
    part.addProLine( proLine );
  }
}