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

import org.kalypso.model.wspm.ewawi.data.enums.EwawiObjectart;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiPunktart;


/**
 * Represents the contents of one EWAWI+ .sta file.
 * 
 * @author Gernot Belger
 */
public class EwawiSta
{
  private File m_sourceFile;

  private final List<EwawiStaLine> m_staLines;

  public EwawiSta( )
  {
    m_sourceFile = null;
    m_staLines = new ArrayList<>();
  }

  public File getSourceFile( )
  {
    return m_sourceFile;
  }

  public EwawiStaLine[] getStaLines( )
  {
    return m_staLines.toArray( new EwawiStaLine[] {} );
  }

  public void setSourceFile( final File sourceFile )
  {
    m_sourceFile = sourceFile;
  }

  public void addStaLine( final EwawiStaLine staLine )
  {
    m_staLines.add( staLine );
  }

  public EwawiStaLine findFixPoint( final EwawiObjectart objectArt, final EwawiPunktart punktArt, final Long gewKennzahl, final BigDecimal station )
  {
    for( final EwawiStaLine staLine : m_staLines )
    {
      if( objectArt == staLine.getObjectArt() && punktArt == staLine.getPunktArt() )
      {
        if( gewKennzahl.equals( staLine.getGewKennzahl() ) && station.equals( staLine.getStation() ) )
          return staLine;
      }
    }

    return null;
  }
}