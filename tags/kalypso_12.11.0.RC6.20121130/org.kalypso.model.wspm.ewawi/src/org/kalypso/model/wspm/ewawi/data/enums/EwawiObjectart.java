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
package org.kalypso.model.wspm.ewawi.data.enums;

/**
 * @author Gernot Belger
 */
public enum EwawiObjectart
{
  _1100( 1100, "Querprofile im Flie�gew�sser", "Alle Querprofile im Flie�gew�sser" ),
  _1200( 1200, "Stehendes Gew�sser", "" ),
  _2000( 2000, "Damm", "Damm, gem�� Vorgabe LfU" ),
  _2100( 2100, "Deich", "Deich, gem�� Vorgabe LfU" ),
  _2200( 2200, "Sonstige L�ngsstruktur", "nicht zum Zwecke des planm��gen Hochwasserschutz gebaute Schutzeinrichtungen (Radwege, Mauern, ...)" ),
  _2300( 2300, "Hochwasserschutzmauer, fest", "" ),
  _2400( 2400, "Hochwasserschutzmauer, mobil", "" ),
  _2500( 2500, "Uferlinie", "Schnittpunkt Wasserspiegel/Gel�nde beidseits" ),
  _2600( 2600, "B�schungsoberkante (Linien)", "beidseits" ),
  _2700( 2700, "Sonstige Bruchkante", "" ),
  _2800( 2800, "Talsperre, Hochwasserr�ckhaltebecken", "" ),
  _2900( 2900, "Durchlass (ausserhalb des Gew�ssers)", "Nur Durchl�sse au�erhalb des Gew�ssers" ),
  _3000( 3000, "sonstige Einzelpunkte", "" ),
  _4000( 4000, "Nodestring Modellzulauf", "" ),
  _4100( 4100, "Nodestring Modellablauf (W-Q)", "" ),
  _4200( 4200, "Nodestring Modellablauf (Energielinien)", "" ),
  _4300( 4300, "Nodestring Kontrollquerschnitt", "" ),
  _4400( 4400, "Nodestring Wehr�berfall", "" ),
  _4500( 4500, "Nodestring Zulauf gebunden an Auslauf", "" ),
  _4600( 4600, "Nodestring Auslauf W-Q/Wehr", "" );

  private final int m_key;

  private final String m_label;

  private final String m_comment;

  EwawiObjectart( final int key, final String label, final String comment )
  {
    m_key = key;
    m_label = label;
    m_comment = comment;
  }

  public int getKey( )
  {
    return m_key;
  }

  public String getLabel( )
  {
    return m_label;
  }

  public String getComment( )
  {
    return m_comment;
  }
}
