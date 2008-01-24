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
package org.kalypso.model.wspm.tuhh.core;

import javax.xml.namespace.QName;

import org.kalypso.model.wspm.core.IWspmConstants;

/**
 * Contains constants for the wspm models.
 * 
 * @author thuel2
 */
public interface IWspmTuhhConstants extends IWspmConstants
{
  public static String NS_WSPM_TUHH = "org.kalypso.model.wspm.tuhh";

  public static final QName Q_WEHRART = new QName( "urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents", "WEHRART" );

  /**
   * The scale (i.e. fraction digits) for station values.
   * 
   * @see BigDecimal
   */
  public static final int STATION_SCALE = 4;

  /** The encoding used by the Kalypso-1D.exe. */
  public static final String WSPMTUHH_CODEPAGE = "Cp1252";

  /*
   * sind Tuhh-Konstanten werden aber wegen der Abwärtskompatibilität in
   * {@link org.kalypso.model.wspm.schema.dict_profile_point} geführt
   */

  public static final String POINTMARKER_PROPERTY = "urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerProperty#";

  public static final String MARKER_TYP = "urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#";

  public static final String BUILDING_PROPERTY = "urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents#";

  public static final String BUILDING_TYP = "urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingTypes#";

  public static final String POINT_PROPERTY = IWspmConstants.POINT_PROPERTY;

  /*----------------------*/

  /* POINT_PROPERTY */
  public static final String POINT_PROPERTY_OBERKANTEBRUECKE = POINT_PROPERTY + "OBERKANTEBRUECKE";

  public static final String POINT_PROPERTY_OBERKANTEWEHR = POINT_PROPERTY + "OBERKANTEWEHR";

  public static final String POINT_PROPERTY_UNTERKANTEBRUECKE = POINT_PROPERTY + "UNTERKANTEBRUECKE";

  /* BUILDING_PROPERTY */
  public static final String BUILDING_PROPERTY_BEZUGSPUNKT_X = BUILDING_PROPERTY + "BEZUGSPUNKT_X";

  public static final String BUILDING_PROPERTY_BEZUGSPUNKT_Y = BUILDING_PROPERTY + "BEZUGSPUNKT_Y";

  public static final String BUILDING_PROPERTY_BREITE = BUILDING_PROPERTY + "BREITE";

  public static final String BUILDING_PROPERTY_FORMBEIWERT = BUILDING_PROPERTY + "FORMBEIWERT";

  public static final String BUILDING_PROPERTY_HOEHE = BUILDING_PROPERTY + "HOEHE";

  public static final String BUILDING_PROPERTY_RAUHEIT = BUILDING_PROPERTY + "RAUHEIT";

  public static final String BUILDING_PROPERTY_SOHLGEFAELLE = BUILDING_PROPERTY + "SOHLGEFAELLE";

  public static final String BUILDING_PROPERTY_STEIGUNG = BUILDING_PROPERTY + "STEIGUNG";

  public static final String BUILDING_PROPERTY_UNTERWASSER = BUILDING_PROPERTY + "UNTERWASSER";

  public static final String BUILDING_PROPERTY_WEHRART = BUILDING_PROPERTY + "WEHRART";

  /* BUILDING_TYP */
  public static final String BUILDING_TYP_BRUECKE = BUILDING_TYP + "BRUECKE";

  public static final String BUILDING_TYP_EI = BUILDING_TYP + "EI";

  public static final String BUILDING_TYP_KREIS = BUILDING_TYP + "KREIS";

  public static final String BUILDING_TYP_MAUL = BUILDING_TYP + "MAUL";

  public static final String BUILDING_TYP_TRAPEZ = BUILDING_TYP + "TRAPEZ";

  public static final String BUILDING_TYP_WEHR = BUILDING_TYP + "WEHR";

  /* MARKER_TYP */
  public static final String MARKER_TYP_BORDVOLL = MARKER_TYP + "BORDVOLL";

  public static final String MARKER_TYP_DURCHSTROEMTE = MARKER_TYP + "DURCHSTROEMTE";

  public static final String MARKER_TYP_TRENNFLAECHE = MARKER_TYP + "TRENNFLAECHE";

  public static final String MARKER_TYP_WEHR = MARKER_TYP + "WEHR";

  /* POINTMARKER_PROPERTY */
  public static final String POINTMARKER_PROPERTY_BEIWERT = POINTMARKER_PROPERTY + "BEIWERT";

  public static final String POINTMARKER_PROPERTY_BOESCHUNG = POINTMARKER_PROPERTY + "BOESCHUNG";

  public static final String POINTMARKER_PROPERTY_RAUHEIT = POINTMARKER_PROPERTY + "RAUHEIT";

  /* Profil MetaStrings */
  public static final String PROFIL_PROPERTY = "org.kalypso.model.wspm.tuhh.core.profil.PROFIL_PROPERTY_";

  public static final String PROFIL_PROPERTY_MEHRFELDBRUECKE = PROFIL_PROPERTY + "MEHRFELDBRUECKE";

  public static final String PROFIL_PROPERTY_METASTRINGS = PROFIL_PROPERTY + "METASTRINGS";

  public static final String PROFIL_PROPERTY_STATUS = PROFIL_PROPERTY + "STATUS";

  public static final String PROFIL_PROPERTY_VERZWEIGUNGSKENNUNG = PROFIL_PROPERTY + "VERZWEIGUNGSKENNUNG";

  public static final String PROFIL_PROPERTY_WASSERSPIEGEL = PROFIL_PROPERTY + "WASSERSPIEGEL";

  /* Rauheiten */
// public static final String RAUHEIT_TYP = "org.kalypso.model.wspm.tuhh.core.profil.RAUHEIT_TYP_";
//
// public static final String RAUHEIT_TYP_KS = RAUHEIT_TYP + "KS";
//
// public static final String RAUHEIT_TYP_KST = RAUHEIT_TYP + "KST";
//
// public static final String DEFAULT_RAUHEIT_TYP = RAUHEIT_TYP_KS;
  /* Wehrtypen */
  /* MUST keep this -wrong- id string in order to be backwards-compatible! */
  public static final String WEHR_TYP = "org.kalypso.model.wspm.core.profil.IProfil.WEHR_TYP_";

  public static final String WEHR_TYP_BEIWERT = WEHR_TYP + "BEIWERT";

  public static final String WEHR_TYP_BREITKRONIG = WEHR_TYP + "BREITKRONIG";

  public static final String WEHR_TYP_RUNDKRONIG = WEHR_TYP + "RUNDKRONIG";

  public static final String WEHR_TYP_SCHARFKANTIG = WEHR_TYP + "SCHARFKANTIG";

  /* Layer */
  public static String PROFILE_LAYER = "org.kalypso.model.wspm.tuhh.ui.chart.LAYER_";

  public static String LAYER_BEWUCHS = PROFILE_LAYER + "BEWUCHS";

  public static String LAYER_GEOKOORDINATEN = PROFILE_LAYER + "GEOKOORDINATEN";

  public static String LAYER_GELAENDE = PROFILE_LAYER + "GELAENDE";

  public static String LAYER_WASSERSPIEGEL = PROFILE_LAYER + "WASSERSPIEGEL";

  public static String LAYER_RAUHEIT_KST = PROFILE_LAYER + "RAUHEIT_KST";

  public static String LAYER_RAUHEIT_KS = PROFILE_LAYER + "RAUHEIT_KS";

  public static String LAYER_RAUHEIT_QUICKVIEW = PROFILE_LAYER + "RAUHEIT_QUICKVIEW";

  public static String LAYER_BRUECKE = PROFILE_LAYER + "BRUECKE";

  public static String LAYER_WEHR = PROFILE_LAYER + "WEHR";

  public static String LAYER_KREIS = PROFILE_LAYER + "KREIS";

  public static String LAYER_MAUL = PROFILE_LAYER + "MAUL";

  public static String LAYER_TRAPEZ = PROFILE_LAYER + "TRAPEZ";

  public static String LAYER_DEVIDER = PROFILE_LAYER + "DEVIDER";

  public static String LAYER_EI = PROFILE_LAYER + "EI";

  /* Layer_Colors */
  /**
   * the layer store their colors themselves in the
   * {@link org.kalypso.model.wspm.core.profil.impl.ProfilEventManager#getColorRegistry()} using this keys
   */

// public static String LAYER_BRUECKE_COLOR_TOP = LAYER_BRUECKE + "COLOR_TOP";
//
// public static String LAYER_BRUECKE_COLOR_BOTTOM = LAYER_BRUECKE + "COLOR_BOTTOM";
  public static String PROFIL_TYPE_PASCHE = "org.kalypso.model.wspm.tuhh.profiletype";

}
