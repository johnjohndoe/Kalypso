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
import java.util.ArrayList;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

import org.kalypso.model.wspm.ewawi.data.enums.EwawiObjectart;
import org.kalypso.model.wspm.ewawi.utils.structures.EwawiLengthStructure;

/**
 * Represents the contents of one EWAWI+ .epl file.
 * 
 * @author Holger Albert
 */
public class EwawiEpl
{
  private File m_sourceFile;

  private final List<EwawiEplLine> m_eplLines;

  public EwawiEpl( )
  {
    m_sourceFile = null;
    m_eplLines = new ArrayList<>();
  }

  public File getSourceFile( )
  {
    return m_sourceFile;
  }

  public EwawiEplLine[] getEplLines( )
  {
    return m_eplLines.toArray( new EwawiEplLine[] {} );
  }

  public void setSourceFile( final File sourceFile )
  {
    m_sourceFile = sourceFile;
  }

  public void addEplLine( final EwawiEplLine eplLine )
  {
    m_eplLines.add( eplLine );
  }

  public EwawiLengthStructure[] getLengthStructures( )
  {
    final SortedMap<Short, EwawiLengthStructure> structures = new TreeMap<>();

    for( final EwawiEplLine eplLine : m_eplLines )
    {
      final EwawiObjectart objectArt = eplLine.getObjectArt();
      if( EwawiObjectart._3000 == objectArt )
        continue;

      final Short objektNummer = eplLine.getObjektNummer();
      if( !structures.containsKey( objektNummer ) )
        structures.put( objektNummer, new EwawiLengthStructure( objektNummer ) );

      final EwawiLengthStructure structure = structures.get( objektNummer );
      structure.addEplLine( eplLine );
    }

    return structures.values().toArray( new EwawiLengthStructure[] {} );
  }
}