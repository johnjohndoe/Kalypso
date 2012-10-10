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
package org.kalypso.model.wspm.pdb.gaf;

import org.eclipse.swt.graphics.RGB;
import org.kalypso.contribs.eclipse.swt.ColorUtilities;

/**
 * @author Holger Albert
 */
public enum GafPointCode
{
  PA( 0, "6010", "Profilanfang", "PA", "P", "", false ),
  PP( 1, "6000", "Profilpunkt", "", "P", "", true ),
  LBOK( 2, "917", "Linke Böschungsoberkante", "LBOK", "P", "", false ),
  LBUK( 3, "918", "Linke Böschungsunterkante", "LBUK", "P", "", false ),
  PST( 4, "970", "Stützmauer", "", "P", "", false ),
  PBA( 5, "", "Profilpunkt Bogenanfang", "", "P", "", false ),
  PBFS( 6, "", "Profilpunkt Bogenpunkt", "", "P", "", false ),
  PBE( 7, "", "Profilpunkt Bogenende", "", "P", "", false ),
  LDOK( 8, "6020", "Linke Deichoberkante", "", "P", "", false ),
  LDUK( 9, "6030", "Linker Deichfuß", "", "P", "", false ),
  LU( 10, "6040", "Linke Uferoberkante", "LU", "P", "", false ),
  FS( 11, "6050", "Feste Sohle", "", "P", "#00FFFF", false ),
  RU( 12, "6060", "Rechte Uferoberkante", "RU", "P", "", false ),
  RBUK( 13, "918", "Rechte Böschungsunterkante", "RBUK", "P", "", false ),
  RBOK( 14, "917", "Rechte Böschungsoberkante", "RBOK", "P", "", false ),
  RDUK( 15, "6070", "Rechter Deichfuß", "", "P", "", false ),
  RDOK( 16, "6080", "Rechte Deichoberkante", "", "P", "", false ),
  PE( 17, "6090", "Profilende", "PE", "P", "", false ),
  SOA( 18, "6100", "Schlammsohle (Anfang)", "", "S", "", false ),
  SOP( 19, "6100", "Schlammsohlenpunkt", "", "S", "", true ),
  SOE( 20, "6100", "Schlammsohle (Ende)", "", "S", "", false ),
  WS( 21, "6200", "Wasserspiegel", "", "W", "", true ),
  SPP0_8( 22, "7000", "Allgem. Bauwerkspunkt", "", "A", "", true ),
  UKAN( 23, "7101", "Beginn Bauwerksunterkante", "", "UK", "", false ),
  UKPP( 24, "7100", "Punkt der Bauwerksunterkante", "", "UK", "", true ),
  UKBA( 25, "7100", "Bauwerksunterkante Bogenanfang", "", "UK", "", false ),
  UKWP( 26, "7102", "Bauwerksunterkante Bogenwendepunkt Ende Bogen 1", "", "UK", "", false ),
  UKBW( 27, "7102", "Bauwerksunterkante Bogenwendepunkt Anfang Bogen 2", "", "UK", "", false ),
  UKBP( 28, "7103", "Bauwerksunterkante Bogenpunkt", "", "UK", "", false ),
  UKBE( 29, "7104", "Bauwerksunterkante Bogenende", "", "UK", "", false ),
  UKEN( 30, "7109", "Ende Bauwerksunterkante", "", "UK", "", false ),
  KRUK( 31, "", "Kreisdurchlass - Unterkante", "", "K", "", true ),
  KRFS( 32, "", "Kreisdurchlass - tiefster Sohlpunkt", "", "K", "", false ),
  EIUK( 33, "", "Ei-Norm-Profil - Unterkante", "", "EI", "", true ),
  EIFS( 34, "", "Ei-Norm-Profil - Unterkante tiefster Sohlpunkt", "", "EI", "", false ),
  MAUK( 35, "", "Maul-Norm-Profil - Unterkante", "", "MA", "", true ),
  MAFS( 36, "", "Maul-Norm-Profil - tiefster Sohlpunkt", "", "MA", "", false ),
  ARUK( 37, "", "ARMCO71-Profil - Unterkante", "", "AR", "", true ),
  ARFS( 38, "", "ARMCO71-Profil - tiefster Sohlpunkt", "", "AR", "", false ),
  ARLR( 39, "", "ARMCO71-Profil - linker Randpunkt", "", "AR", "", false ),
  ARRR( 40, "", "ARMCO71-Profil - rechter Randpunkt", "", "AR", "", false ),
  HAUK( 41, "", "HAMCO84-Profil - Unterkante", "", "HA", "", true ),
  HAFS( 42, "", "HAMCO84-Profil - tiefster Sohlpunkt", "", "HA", "", false ),
  HALR( 43, "", "HAMCO84-Profil - linker Randpunkt", "", "HA", "", false ),
  HARR( 44, "", "HAMCO84-Profil - rechter Randpunkt", "", "HA", "", false ),
  OKAN( 45, "7201", "Bauwerkoberkante Anfang", "", "OK", "", false ),
  OKPP( 46, "7200", "Bauwerkoberkante allgem. Punkt", "", "OK", "", true ),
  OKBA( 47, "7200", "Bauwerkoberkante allgem. Punkt", "", "OK", "", false ),
  OKBW( 48, "7202", "Bauwerkoberkante Wendepunkt", "", "OK", "", false ),
  OKBP( 49, "7203", "Bauwerkoberkante Bogenpunkt", "", "OK", "", false ),
  OKBE( 50, "7204", "Bauwerkoberkante Bogenende", "", "OK", "", false ),
  OKEN( 51, "7209", "Bauwerkoberkante Ende", "", "OK", "", false ),
  PWSP( 52, "0", "Berechnete Wasserspiegellage", "", "W", "", false );

  private final int m_number;

  private final String m_dbCode;

  private final String m_description;

  private final String m_hyk;

  private final GafKind m_kind;

  private final RGB m_color;

  private final boolean m_isDefault;

  private GafPointCode( final int number, final String dbCode, final String description, final String hyk, final String kind, final String color, final boolean isDefault )
  {
    m_number = number;
    m_dbCode = dbCode;
    m_description = description;
    m_hyk = hyk;
    m_kind = GafKind.valueOf( kind );
    m_color = ColorUtilities.toRGBFromHTML( color );
    m_isDefault = isDefault;
  }

  public String getKey( )
  {
    return name().replace( '_', '-' );
  }

  public int getNumber( )
  {
    return m_number;
  }

  public String getDbCode( )
  {
    return m_dbCode;
  }

  public String getDescription( )
  {
    return m_description;
  }

  public String getHyk( )
  {
    return m_hyk;
  }

  public GafKind getKind( )
  {
    return m_kind;
  }

  public RGB getColor( )
  {
    return m_color;
  }

  public boolean isDefault( )
  {
    return m_isDefault;
  }

  @Override
  public String toString( )
  {
    return m_description;
  }
}