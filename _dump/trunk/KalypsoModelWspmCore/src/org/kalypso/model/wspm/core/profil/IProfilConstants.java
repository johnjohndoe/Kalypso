/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.core.profil;

public interface IProfilConstants
{
  /** Format String für Formatierung der Station in der GUI */
  public static final String FMT_STATION = "%.4f";

  /**
   * profilProperties
   */
  /** ObjectTyp=List<String> */
  public static final String PROFIL_PROPERTY = "org.kalypso.model.wspm.core.profil.IProfil.PROFIL_PROPERTY_";

  public static final String PROFIL_PROPERTY_KOMMENTAR = PROFIL_PROPERTY + "KOMMENTAR";

  // /** ObjectTyp=String */
  public static final String PROFIL_PROPERTY_MEHRFELDBRUECKE = PROFIL_PROPERTY + "MEHRFELDBRUECKE";

  // /** ObjectTyp=List<String> */
  public static final String PROFIL_PROPERTY_METASTRINGS = PROFIL_PROPERTY + "METASTRINGS";

  // /** ObjectTyp=String */
  public static final String PROFIL_PROPERTY_STATUS = PROFIL_PROPERTY + "STATUS";

  // /** ObjectTyp=String */
  public static final String PROFIL_PROPERTY_VERZWEIGUNGSKENNUNG = PROFIL_PROPERTY + "VERZWEIGUNGSKENNUNG";

  // /** ObjectTyp=String */
  public static final String PROFIL_PROPERTY_WASSERSPIEGEL = PROFIL_PROPERTY + "WASSERSPIEGEL";

  /** Rauhheit */
  public static final String RAUHEIT_TYP = "org.kalypso.model.wspm.core.profil.IProfil.RAUHEIT_TYP_";

  public static final String RAUHEIT_TYP_KS = RAUHEIT_TYP + "KS";

  public static final String RAUHEIT_TYP_KST = RAUHEIT_TYP + "KST";

  /** default RauheitenTyp für ein neues Profil */
  public static final String DEFAULT_RAUHEIT_TYP = RAUHEIT_TYP_KS;

  /** Wehr-typen */
  public static final String WEHR_TYP = "org.kalypso.model.wspm.core.profil.IProfil.WEHR_TYP_";

  public static final String WEHR_TYP_RUNDKRONIG = WEHR_TYP + "RUNDKRONIG";

  public static final String WEHR_TYP_BREITKRONIG = WEHR_TYP + "BREITKRONIG";

  public static final String WEHR_TYP_SCHARFKANTIG = WEHR_TYP + "SCHARFKANTIG";

  public static final String WEHR_TYP_BEIWERT = WEHR_TYP + "BEIWERT";

  /** Trenner-typen */
  public static final String DEVIDER_TYP = "urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#";

  public static final String DEVIDER_TYP_BORDVOLL = DEVIDER_TYP + "BORDVOLL";

  public static final String DEVIDER_TYP_DURCHSTROEMTE = DEVIDER_TYP + "DURCHSTROEMTE";

  public static final String DEVIDER_TYP_TRENNFLAECHE = DEVIDER_TYP + "TRENNFLAECHE";

  public static final String DEVIDER_TYP_WEHR = DEVIDER_TYP + "WEHR";

  public static final String[] DEVIDER_TYPES = { DEVIDER_TYP_BORDVOLL, DEVIDER_TYP_DURCHSTROEMTE, DEVIDER_TYP_TRENNFLAECHE, DEVIDER_TYP_WEHR };

  /** buildings */
  public static final String BUILDING_TYP = "urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingTypes#";

  public static final String BUILDING_TYP_BRUECKE = BUILDING_TYP + "BRUECKE";

  public static final String BUILDING_TYP_EI = BUILDING_TYP + "EI";

  public static final String BUILDING_TYP_KREIS = BUILDING_TYP + "KREIS";

  public static final String BUILDING_TYP_MAUL = BUILDING_TYP + "MAUL";

  public static final String BUILDING_TYP_TRAPEZ = BUILDING_TYP + "TRAPEZ";

  public static final String BUILDING_TYP_WEHR = BUILDING_TYP + "WEHR";

}
