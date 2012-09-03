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
package org.kalypso.model.wspm.ewawi.data;

import java.io.File;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfile;

/**
 * Represents the contents of one EWAWI+ .pro file.
 * 
 * @author Holger Albert
 */
public class EwawiPro
{
  private File m_sourceFile;

  private final List<EwawiProLine> m_proLines;

  public EwawiPro( )
  {
    m_sourceFile = null;
    m_proLines = new ArrayList<>();
  }

  public File getSourceFile( )
  {
    return m_sourceFile;
  }

  public EwawiProLine[] getProLines( )
  {
    return m_proLines.toArray( new EwawiProLine[] {} );
  }

  public void setSourceFile( final File sourceFile )
  {
    m_sourceFile = sourceFile;
  }

  public void addProLine( final EwawiProLine proLine )
  {
    m_proLines.add( proLine );
  }

  public EwawiProfile[] getProfiles( )
  {
    final SortedMap<BigDecimal, EwawiProfile> profiles = new TreeMap<>();

    for( final EwawiProLine proLine : m_proLines )
    {
      final BigDecimal station = proLine.getStation();
      if( !profiles.containsKey( station ) )
        profiles.put( station, new EwawiProfile( station ) );

      final EwawiProfile profile = profiles.get( station );
      profile.addProLine( proLine );
    }

    return profiles.values().toArray( new EwawiProfile[] {} );
  }
}