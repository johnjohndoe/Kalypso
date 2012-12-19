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
package org.kalypso.model.wspm.ewawi.data.reader;

import java.math.BigDecimal;
import java.text.ParseException;
import java.util.Date;

import org.kalypso.model.wspm.ewawi.data.EwawiPlus;
import org.kalypso.model.wspm.ewawi.data.EwawiStaLine;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiObjectart;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiProfilart;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiPunktart;

/**
 * Reads EWAWI+ .sta files.
 * 
 * @author Gernot Belger
 */
public class EwawiStaReader extends AbstractEwawiReader
{
  public EwawiStaReader( final EwawiPlus data )
  {
    super( data );
  }

  @Override
  protected void readTabs( final String[] tabs ) throws ParseException
  {
    // a Objektart (siehe Schl�sselliste Objektart, Querprofile immer 1100)
    final EwawiObjectart objectArt = asObjectArt( tabs[0] );

    // b Gew�sserkennzahl siehe Gew�sserkulisse
    final String gewKennzahlText = tabs[1];
    final Long gewKennzahl = getGewKennzahl( gewKennzahlText );

    // c Flusskilometer siehe FKM-Tafeln
    final BigDecimal station = asDecimal( tabs[2] );

    // d Zusatzkennzahl Nebengew�sser, wenn GKZ gleich, Standard 0
    final Short zusatz = asShort( tabs[3] );

    // e Punktnummer unsortiert
    final Short punktNummer = asShort( tabs[4] );

    // f Punktart (siehe Schl�sselliste Punktart)
    final EwawiPunktart punktArt = asPunktart( tabs[5] );

    // g Rechtswert
    final BigDecimal rechtswert = asDecimal( tabs[6] );

    // h Hochwert
    final BigDecimal hochwert = asDecimal( tabs[7] );

    // i H�he
    final BigDecimal hoehe = asDecimal( tabs[8] );

    // j G�ltigkeitsdatum
    final Date validity = asDate( tabs[9] );

    // k linke Berechnungsgrenze nicht relevant f�r HWGK
    // as( tabs[10] );

    // l rechte Berechnungsgrenze nicht relevant f�r HWGK
    // as( tabs[11] );

    // m Obere Berechnungsgrenze nicht relevant f�r HWGK
    // as( tabs[12] );

    // n Abstand zum oberstromigen Profil nicht relevant f�r HWGK
    // as( tabs[13] );

    // o Bemerkung Hinweise f�r den Bearbeiter Hydraulik
    final String comment = asString( tabs[14] );

    // p Profilart (siehe Schl�sselliste Profilart)
    final EwawiProfilart profilart = asProfilart( tabs[15] );

    // q Profilnummer (in 10er-Schritten aufsteigend von der M�ndung zu Quelle) Zwischennummern sind m�glich
    final short profilNummer = asShort( tabs[16] );

    // r Bildname Dateiname des zugeh�rigen Fotos, bei mehreren Bildern je Profil sind diese mit Komma ohne Leerzeichen zu trennen. Die Bilder sind an allen Eintr�gen eines Profils zu wiederholen.
    final String[] photos = asStringArray( tabs[17], ',' );

    final EwawiStaLine staLine = new EwawiStaLine( objectArt, gewKennzahl, station, zusatz, punktNummer, punktArt, rechtswert, hochwert, hoehe, validity, comment, profilart, profilNummer, photos );
    getData().addStaLine( staLine );
  }
}