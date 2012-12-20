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
public enum EwawiProfilart
{
  _1( 1, "Gew�sserprofil an Flusskilometerstein", "Regelprofil" ),
  _2( 2, "Gew�sserprofil", "sonstige Profile" ),
  _3( 3, "Br�cke", "Br�ckenprofile, Verbundprofil" ),
  _4( 4, "Absturz, Rampe", "Profil verl�uft an der Absturzkante, Profil nach Absturz ist Gew�sserprofil" ),
  _5( 5, "Verrohrung -Einlauf", "Verbundprofil am Verrohrungseinlauf (f�r Durchl�sse im Gew�sser in Flie�richtung)" ),
  _6( 6, "Verrohrung Auslauf", "Verbundprofil am Verrohrungauslauf (f�r Durchl�sse im Gew�sser in Flie�richtung)" ),
  _7( 7, "D�ker - Einlauf", "" ),
  _8( 8, "D�ker - Auslauf", "" ),
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