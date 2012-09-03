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
public enum EwawiProfilart
{
  _1( 1, "Gewässerprofil an Flusskilometerstein", "Regelprofil" ),
  _2( 2, "Gewässerprofil", "sonstige Profile" ),
  _3( 3, "Brücke", "Brückenprofile, Verbundprofil" ),
  _4( 4, "Absturz, Rampe", "Profil verläuft an der Absturzkante, Profil nach Absturz ist Gewässerprofil" ),
  _5( 5, "Verrohrung -Einlauf", "Verbundprofil am Verrohrungseinlauf (für Durchlässe im Gewässer in Fließrichtung)" ),
  _6( 6, "Verrohrung Auslauf", "Verbundprofil am Verrohrungauslauf (für Durchlässe im Gewässer in Fließrichtung)" ),
  _7( 7, "Düker - Einlauf", "" ),
  _8( 8, "Düker - Auslauf", "" ),
  _9( 9, "Wehr", "Absturbzauwerke mit Aufbauten und/oder Regelorganen" ),
  _10( 10, "Pegelanlage", "an Messstellen" );

  private final int m_key;

  private final String m_label;

  private final String m_comment;

  EwawiProfilart( final int key, final String label, final String comment )
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