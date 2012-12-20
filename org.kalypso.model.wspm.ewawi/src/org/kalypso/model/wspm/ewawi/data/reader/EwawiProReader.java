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
package org.kalypso.model.wspm.ewawi.data.reader;

import java.math.BigDecimal;
import java.text.ParseException;
import java.util.Date;

import org.kalypso.model.wspm.ewawi.data.EwawiPlus;
import org.kalypso.model.wspm.ewawi.data.EwawiProLine;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiHorizont;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiObjectart;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiPunktart;

/**
 * Reads EWAWI+ .pro files.
 * 
 * @author Gernot Belger
 */
public class EwawiProReader extends AbstractEwawiReader
{
  public EwawiProReader( final EwawiPlus data )
  {
    super( data );
  }

  @Override
  protected void readTabs( final String[] tabs ) throws ParseException
  {
    // a Objektart (siehe Schlüsselliste Objektart)
    final EwawiObjectart objectArt = asObjectArt( tabs[0] );

    // b Gewässerkennzahl siehe Gewässerkulisse
    final String gewKennzahlText = tabs[1];
    final Long gewKennzahl = getGewKennzahl( gewKennzahlText );

    // c Flusskilometer siehe FKM-Tafeln
    final BigDecimal station = asDecimal( tabs[2] );

    // d Zusatzkennzahl Aufsteigender Zähler für Parallelläufe
    final Short zusatz = asShort( tabs[3] );

    // e Punktnummer unsortiert
    final Short punktNummer = asShort( tabs[4] );

    // f Punktart (siehe Schlüsselliste Punktarten)
    final EwawiPunktart punktArt = asPunktart( tabs[5] );

    // g Rechtswert (Projektion auf Abszisse) von links nach rechts aufsteigend
    final BigDecimal rechtswert = asDecimal( tabs[6] );

    // h Hochwert (Abweichung von der Achse) rechts positiv, links negativer Wert.
    final BigDecimal hochwert = asDecimal( tabs[7] );

    // i Höhe (relative Höhe bezogen auf linken, wenn nicht vorhanden rechten Festpunkt), Kontrolle der Restklaffen am rechten Festpunkt)
    final BigDecimal hoehe = asDecimal( tabs[8] );

    // j Aufnahmedatum
    final Date aufnahmeDatum = asDate( tabs[9] );

    // k Bedeutung Horizont (siehe Schlüsselliste Horizonte)
    final EwawiHorizont horizont = asHorizont( tabs[10] );

    // l Bemerkung Freitext
    final String comment = asString( tabs[11] );

    // m Punktreihenfolge von links nach rechts aufsteigende Punktnummerierung
    final short punktReihenfolge = asShort( tabs[12] );

    final EwawiProLine proLine = new EwawiProLine( objectArt, gewKennzahl, station, zusatz, punktNummer, punktArt, rechtswert, hochwert, hoehe, aufnahmeDatum, horizont, comment, punktReihenfolge );
    getData().addProLine( proLine );
  }
}