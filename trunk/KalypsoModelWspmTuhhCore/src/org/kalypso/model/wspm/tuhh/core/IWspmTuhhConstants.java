/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.tuhh.core;

import org.kalypso.model.wspm.core.IWspmConstants;

/**
 * Contains constants for the wspm models.
 * 
 * @author thuel2
 */
public interface IWspmTuhhConstants extends IWspmConstants
{
  String NS_WSPM_TUHH = "org.kalypso.model.wspm.tuhh"; //$NON-NLS-1$

  /**
   * The scale (i.e. fraction digits) for station values.
   * 
   * @see BigDecimal
   */
  int STATION_SCALE = 4;

  /** The encoding used by the Kalypso-1D.exe. */
  String WSPMTUHH_CODEPAGE = "Cp1252"; //$NON-NLS-1$

  /*
   * sind Tuhh-Konstanten werden aber wegen der Abw�rtskompatibilit�t in {@link
   * org.kalypso.model.wspm.schema.dict_profile_point} gef�hrt
   */

  String MARKER_TYP = "urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#"; //$NON-NLS-1$

  String BUILDING_TYP = "urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingTypes#"; //$NON-NLS-1$

  String POINT_PROPERTY = IWspmConstants.POINT_PROPERTY;

  /*----------------------*/

  /* POINT_PROPERTY */
  String POINT_PROPERTY_OBERKANTEBRUECKE = POINT_PROPERTY + "OBERKANTEBRUECKE"; //$NON-NLS-1$

  String POINT_PROPERTY_OBERKANTEWEHR = POINT_PROPERTY + "OBERKANTEWEHR"; //$NON-NLS-1$

  String POINT_PROPERTY_UNTERKANTEBRUECKE = POINT_PROPERTY + "UNTERKANTEBRUECKE"; //$NON-NLS-1$

  /* BUILDING_TYP */
  String BUILDING_TYP_BRUECKE = BUILDING_TYP + "BRUECKE"; //$NON-NLS-1$

  String BUILDING_TYP_EI = BUILDING_TYP + "EI"; //$NON-NLS-1$

  String BUILDING_TYP_KREIS = BUILDING_TYP + "KREIS"; //$NON-NLS-1$

  String BUILDING_TYP_MAUL = BUILDING_TYP + "MAUL"; //$NON-NLS-1$

  String BUILDING_TYP_TRAPEZ = BUILDING_TYP + "TRAPEZ"; //$NON-NLS-1$

  String BUILDING_TYP_WEHR = BUILDING_TYP + "WEHR"; //$NON-NLS-1$

  /* MARKER_TYP */
  String MARKER_TYP_BORDVOLL = MARKER_TYP + "BORDVOLL"; //$NON-NLS-1$

  String MARKER_TYP_DURCHSTROEMTE = MARKER_TYP + "DURCHSTROEMTE"; //$NON-NLS-1$

  String MARKER_TYP_TRENNFLAECHE = MARKER_TYP + "TRENNFLAECHE"; //$NON-NLS-1$

  String MARKER_TYP_WEHR = MARKER_TYP + "WEHR"; //$NON-NLS-1$

  /* Profil MetaStrings */
  String PROFIL_PROPERTY = "org.kalypso.model.wspm.tuhh.core.profil.PROFIL_PROPERTY_"; //$NON-NLS-1$

  String PROFIL_PROPERTY_MEHRFELDBRUECKE = PROFIL_PROPERTY + "MEHRFELDBRUECKE"; //$NON-NLS-1$

  String PROFIL_PROPERTY_METASTRINGS = PROFIL_PROPERTY + "METASTRINGS"; //$NON-NLS-1$

  String PROFIL_PROPERTY_STATUS = PROFIL_PROPERTY + "STATUS"; //$NON-NLS-1$

  String PROFIL_PROPERTY_VERZWEIGUNGSKENNUNG = PROFIL_PROPERTY + "VERZWEIGUNGSKENNUNG"; //$NON-NLS-1$

  String PROFIL_PROPERTY_WASSERSPIEGEL = PROFIL_PROPERTY + "WASSERSPIEGEL"; //$NON-NLS-1$

  /* Layer */
  String PROFILE_LAYER = "org.kalypso.model.wspm.tuhh.ui.chart.LAYER_"; //$NON-NLS-1$

  String LAYER_BEWUCHS = PROFILE_LAYER + "BEWUCHS"; //$NON-NLS-1$

  // String LAYER_GEOKOORDINATEN = PROFILE_LAYER + "GEOKOORDINATEN"; //$NON-NLS-1$

  // String LAYER_GELAENDE = PROFILE_LAYER + "GELAENDE"; //$NON-NLS-1$

  // String LAYER_WASSERSPIEGEL = PROFILE_LAYER + "WASSERSPIEGEL"; //$NON-NLS-1$

  String LAYER_RAUHEIT = PROFILE_LAYER + "RAUHEIT"; //$NON-NLS-1$

  // String LAYER_RAUHEIT_KS = PROFILE_LAYER + "RAUHEIT_KS";

  //String LAYER_RAUHEIT_QUICKVIEW = PROFILE_LAYER + "RAUHEIT_QUICKVIEW"; //$NON-NLS-1$

  String LAYER_BRUECKE = PROFILE_LAYER + "BRUECKE"; //$NON-NLS-1$

  String LAYER_WEHR = PROFILE_LAYER + "WEHR"; //$NON-NLS-1$

  String LAYER_KREIS = PROFILE_LAYER + "KREIS"; //$NON-NLS-1$

  String LAYER_MAUL = PROFILE_LAYER + "MAUL"; //$NON-NLS-1$

  String LAYER_TRAPEZ = PROFILE_LAYER + "TRAPEZ"; //$NON-NLS-1$

  String LAYER_DEVIDER = PROFILE_LAYER + "DEVIDER"; //$NON-NLS-1$

  String LAYER_EI = PROFILE_LAYER + "EI"; //$NON-NLS-1$

  String LAYER_TUBES = PROFILE_LAYER + "TUBES"; //$NON-NLS-1$

  String LAYER_SINUOSITAET = PROFILE_LAYER + "SINUOSITAET"; //$NON-NLS-1$

  String LAYER_ENERGYLOSS = PROFILE_LAYER + "ENERGYLOSS"; //$NON-NLS-1$

//  String LAYER_WASSERSPIEGEL2D = PROFILE_LAYER + "WASSERPIEGEL2D"; //$NON-NLS-1$

  String PROFIL_TYPE_PASCHE = "org.kalypso.model.wspm.tuhh.profiletype"; //$NON-NLS-1$

  // Names of commonly used directories
  String FOLDER_RESULTS = "Ergebnisse"; //$NON-NLS-1$

  String FOLDER_CURRENT_RESULT = "_aktuell";//$NON-NLS-1$

  String FOLDER_RESULT_DATA = "Daten";//$NON-NLS-1$

  /* Layer_Colors */
  /**
   * the layer store their colors themselves in the {@link org.kalypso.model.wspm.core.profil.impl.ProfilEventManager#getColorRegistry()} using this keys
   */
  String FILE_WSPTIN = "WspTin.gml";//$NON-NLS-1$

  String FILE_MODELL_GML = "modell.gml"; //$NON-NLS-1$

  String FILE_WSPM_GMV = "WSPM.gmv"; //$NON-NLS-1$

  String FILE_RESULT_LENGTH_SECTION_GML = "L�ngsschnitt.gml"; //$NON-NLS-1$

  String FILE_RESULT_POLYNOME_LENGTH_SECTIONS_GML = "lengthSection_*.gml"; //$NON-NLS-1$

  String FILE_PATTERN_POLYNOME_LENGTH_SECTIONS_GML = "lengthSection_(.*).gml"; //$NON-NLS-1$

  String FILE_OVERVIEW_MAP = "�bersichtskarte.gmt"; //$NON-NLS-1$

  String FILE_LAENGSSCHNITT_GML = "L�ngsschnitt.gml";//$NON-NLS-1$ 

  String DIR_RESULT_DATEN = "Daten"; //$NON-NLS-1$
}
