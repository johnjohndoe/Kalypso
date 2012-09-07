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
package org.kalypso.model.wspm.tuhh.ui.imports.ewawi;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kalypso.model.wspm.ewawi.data.EwawiProLine;

/**
 * @author Holger Albert
 */
public class EwawiProfilePointCache
{
  private final Map<BigDecimal, List<EwawiProLine>> m_cache;

  public EwawiProfilePointCache( )
  {
    m_cache = new HashMap<>();
  }

  public BigDecimal[] getStations( )
  {
    return m_cache.keySet().toArray( new BigDecimal[] {} );
  }

  public EwawiProLine[] getProLines( final BigDecimal station )
  {
    final List<EwawiProLine> proLines = m_cache.get( station );
    if( proLines == null )
      return new EwawiProLine[] {};

    return proLines.toArray( new EwawiProLine[] {} );
  }

  public void addProLine( final EwawiProLine proLine )
  {
    final BigDecimal station = proLine.getStation();
    if( !m_cache.containsKey( station ) )
      m_cache.put( station, new ArrayList<EwawiProLine>() );

    final List<EwawiProLine> proLines = m_cache.get( station );
    proLines.add( proLine );
  }
}