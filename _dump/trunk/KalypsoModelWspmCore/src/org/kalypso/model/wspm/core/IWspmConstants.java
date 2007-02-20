/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.model.wspm.core;

import java.math.MathContext;

/**
 * Contains constants for the wspm models.
 * 
 * @author thuel2
 */
public interface IWspmConstants
{
  public static String NS_WSPM = "org.kalypso.model.wspm";

  public static String NS_WSPMCOMMONS = "org.kalypso.model.wspmcommon";

  public static String NS_WSPMPROF = "org.kalypso.model.wspmprofile";

  public static String NS_WSPMPROF_ASSIGNMENT = "org.kalypso.model.wspmprofile.assignment";

  public static String NS_WSPMRUNOFF = "org.kalypso.model.wspmrunoff";

  public static String NS_WSPMPROJ = "org.kalypso.model.wspmproj";

  public static String NS_NA_WSPM = "org.kalypso.model.wspm.nawspm";

  public static String NS_WSPM_BREAKLINE = "org.kalypso.model.wspmbreakline";

  public static String NS_WSPM_BOUNDARY = "org.kalypso.model.wspmboundary";

  // TODO: get number of fractions digits from gml-schema
  public static final String FMT_STATION = "%.4f";

  public static final MathContext STATION_MATH_CONTEXT = new MathContext( 4 );

  public static final String POINT_PROPERTY = "urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#";

  public static final String POINT_PROPERTY_BEWUCHS_AX = POINT_PROPERTY + "BEWUCHS_AX";

  public static final String POINT_PROPERTY_BEWUCHS_AY = POINT_PROPERTY + "BEWUCHS_AY";

  public static final String POINT_PROPERTY_BEWUCHS_DP = POINT_PROPERTY + "BEWUCHS_DP";

  public static final String POINT_PROPERTY_BREITE = POINT_PROPERTY + "BREITE";

  public static final String POINT_PROPERTY_HOCHWERT = POINT_PROPERTY + "HOCHWERT";

  public static final String POINT_PROPERTY_HOEHE = POINT_PROPERTY + "HOEHE";

  public static final String POINT_PROPERTY_RAUHEIT = POINT_PROPERTY + "RAUHEIT";

  public static final String POINT_PROPERTY_RECHTSWERT = POINT_PROPERTY + "RECHTSWERT";

  public static final String PROFIL_PROPERTY = "org.kalypso.model.wspm.core.profil.IProfil.PROFIL_PROPERTY_";

  public static final String PROFIL_PROPERTY_KOMMENTAR = PROFIL_PROPERTY + "KOMMENTAR";

  public static final String PROFIL_PROPERTY_NAME = PROFIL_PROPERTY + "NAME";
}
