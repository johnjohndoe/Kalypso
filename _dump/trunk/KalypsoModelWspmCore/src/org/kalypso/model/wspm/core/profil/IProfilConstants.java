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

  /** Rauhheit */
  public static final String RAUHEIT_TYP_KS = "org.kalypso.model.wspm.core.profil.IProfil.RAUHEIT_TYP_KS";

  public static final String RAUHEIT_TYP_KST = "org.kalypso.model.wspm.core.profil.IProfil.RAUHEIT_TYP_KST";

  /** default RauheitenTyp für ein neues Profil */
  public static final String DEFAULT_RAUHEIT_TYP = RAUHEIT_TYP_KS;

  /** Wehr-typen */
  public static final String WEHR_TYP_RUNDKRONIG = "org.kalypso.model.wspm.core.profil.IProfil.WEHR_TYP_RUNDKRONIG";

  public static final String WEHR_TYP_BREITKRONIG = "org.kalypso.model.wspm.core.profil.IProfil.WEHR_TYP_BREITKRONIG";

  public static final String WEHR_TYP_SCHARFKANTIG = "org.kalypso.model.wspm.core.profil.IProfil.WEHR_TYP_SCHARFKANTIG";

  public static final String WEHR_TYP_BEIWERT = "org.kalypso.model.wspm.core.profil.IProfil.WEHR_TYP_BEIWERT";

  /** buildings */

  public static final String BUILDING_TYP_BRUECKE = "org.kalypso.model.wspm.core.profil.IProfil.BUILDING_TYP_BRUECKE";

  public static final String BUILDING_TYP_EI = "org.kalypso.model.wspm.core.profil.IProfil.BUILDING_TYP_EI";

  public static final String BUILDING_TYP_KREIS = "org.kalypso.model.wspm.core.profil.IProfil.BUILDING_TYP_KREIS";

  public static final String BUILDING_TYP_MAUL = "org.kalypso.model.wspm.core.profil.IProfil.BUILDING_TYP_MAUL";

  public static final String BUILDING_TYP_TRAPEZ = "org.kalypso.model.wspm.core.profil.IProfil.BUILDING_TYP_TRAPEZ";

  public static final String BUILDING_TYP_WEHR = "org.kalypso.model.wspm.core.profil.IProfil.BUILDING_TYP_WEHR";

}
