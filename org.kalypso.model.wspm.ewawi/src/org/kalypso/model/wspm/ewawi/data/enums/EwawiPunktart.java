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
package org.kalypso.model.wspm.ewawi.data.enums;

/**
 * @author Gernot Belger
 */
public enum EwawiPunktart
{
  _0( 0, "Schnittpunkt Wasserspiegel - Gelände", "Schnittpunkt mit Uferlinie" ),
  _1( 1, "Linker Festpunkt", "" ),
  _2( 2, "Rechter Festpunkt", "" ),
  _3( 3, "befestigtes Gelände", "" ),
  _4( 4, "bewegliche Sohle", "" ),
  _5( 5, "bewachsenes Gelände", "" ),
  _6( 6, "Umsetzpunkt", "" ),
  _7( 7, "Befestigte Sohle", "" ),
  _8( 8, "Böschungsunterkante", "" ),
  _9( 9, "Böschungsoberkante", "Punkte, die den \"bordvoll-Abfluss\" markieren" ),
  _10( 10, "Sonderpunkte", "" ),
  _11( 11, "UK Mauer / Sockel", "" ),
  _12( 12, "OK Mauer / Sockel", "" ),
  _13( 13, "UK Deich", "" ),
  _14( 14, "OK Deich", "" ),
  _15( 15, "OK Fahrbahn", "" ),
  _16( 16, "Sohlpunkt Kreisprofil", "Durchlässe, seitliche Einläufe >DN500" ),
  _17( 17, "Straßen- oder Wegrand", "" ),
  _18( 18, "Gleisoberkante", "" ),
  _19( 19, "Bauwerkspunkt allg.", "" ),
  _20( 20, "Fusspunkt Widerlager", "" ),
  _21( 21, "UK Brücke", "" ),
  _22( 22, "OK Brücke", "" ),
  _23( 23, "Fusspunkt Brückenstütze", "" ),
  _24( 24, "Geländer", "" ),
  _25( 25, "Durchlass", "" ),
  _30( 30, "OK Wehr", "" ),
  _31( 31, "UK Wehr", "" ),
  _32( 32, "OK Schütz", "" ),
  _33( 33, "UK Schütz", "" ),
  _35( 35, "Hochwassermarke", "" );

  private final int m_key;

  private final String m_label;

  private final String m_comment;

  EwawiPunktart( final int key, final String label, final String comment )
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
