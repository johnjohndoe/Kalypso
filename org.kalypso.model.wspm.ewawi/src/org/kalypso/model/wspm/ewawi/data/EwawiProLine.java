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

import java.math.BigDecimal;
import java.util.Date;

import org.kalypso.model.wspm.ewawi.data.enums.EwawiHorizont;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiObjectart;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiPunktart;

/**
 * Represents one line of a EWAWI+ .pro file.
 * 
 * @author Holger Albert
 */
public class EwawiProLine
{
  private final EwawiObjectart m_objectArt;

  private final Long m_gewKennzahl;

  private final BigDecimal m_station;

  private final Short m_zusatz;

  private final Short m_punktNummer;

  private final EwawiPunktart m_punktArt;

  private final BigDecimal m_rechtswert;

  private final BigDecimal m_hochwert;

  private final BigDecimal m_hoehe;

  private final Date m_aufnahmeDatum;

  private final EwawiHorizont m_horizont;

  private final String m_comment;

  private final Short m_punktReihenfolge;

  public EwawiProLine( final EwawiObjectart objectArt, final Long gewKennzahl, final BigDecimal station, final Short zusatz, final Short punktNummer, final EwawiPunktart punktArt, final BigDecimal rechtswert, final BigDecimal hochwert, final BigDecimal hoehe, final Date aufnahmeDatum, final EwawiHorizont horizont, final String comment, final Short punktReihenfolge )
  {
    m_objectArt = objectArt;
    m_gewKennzahl = gewKennzahl;
    m_station = station;
    m_zusatz = zusatz;
    m_punktNummer = punktNummer;
    m_punktArt = punktArt;
    m_rechtswert = rechtswert;
    m_hochwert = hochwert;
    m_hoehe = hoehe;
    m_aufnahmeDatum = aufnahmeDatum;
    m_horizont = horizont;
    m_comment = comment;
    m_punktReihenfolge = punktReihenfolge;
  }

  public EwawiObjectart getObjectArt( )
  {
    return m_objectArt;
  }

  public Long getGewKennzahl( )
  {
    return m_gewKennzahl;
  }

  public BigDecimal getStation( )
  {
    return m_station;
  }

  public Short getZusatz( )
  {
    return m_zusatz;
  }

  public Short getPunktNummer( )
  {
    return m_punktNummer;
  }

  public EwawiPunktart getPunktArt( )
  {
    return m_punktArt;
  }

  public BigDecimal getRechtswert( )
  {
    return m_rechtswert;
  }

  public BigDecimal getHochwert( )
  {
    return m_hochwert;
  }

  public BigDecimal getHoehe( )
  {
    return m_hoehe;
  }

  public Date getAufnahmeDatum( )
  {
    return m_aufnahmeDatum;
  }

  public EwawiHorizont getHorizont( )
  {
    return m_horizont;
  }

  public String getComment( )
  {
    return m_comment;
  }

  public Short getPunktReihenfolge( )
  {
    return m_punktReihenfolge;
  }
}