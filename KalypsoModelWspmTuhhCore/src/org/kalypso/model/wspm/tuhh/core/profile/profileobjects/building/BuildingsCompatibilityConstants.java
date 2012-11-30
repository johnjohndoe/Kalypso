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
package org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building;

/**
 * @author Holger Albert
 */
public class BuildingsCompatibilityConstants
{
  private static final String BUILDING_PROPERTY = "urn:ogc:gml:dict:kalypso:model:wspm:profileBuildingComponents#"; //$NON-NLS-1$

  public static String BUILDING_PROPERTY_BEZUGSPUNKT_X = BUILDING_PROPERTY + "BEZUGSPUNKT_X"; //$NON-NLS-1$

  public static String BUILDING_PROPERTY_BEZUGSPUNKT_Y = BUILDING_PROPERTY + "BEZUGSPUNKT_Y"; //$NON-NLS-1$

  public static String BUILDING_PROPERTY_BREITE = BUILDING_PROPERTY + "BREITE"; //$NON-NLS-1$

  public static String BUILDING_PROPERTY_FORMBEIWERT = BUILDING_PROPERTY + "FORMBEIWERT"; //$NON-NLS-1$

  public static String BUILDING_PROPERTY_HOEHE = BUILDING_PROPERTY + "HOEHE"; //$NON-NLS-1$

  public static String BUILDING_PROPERTY_RAUHEIT = BUILDING_PROPERTY + "RAUHEIT"; //$NON-NLS-1$

  public static String BUILDING_PROPERTY_SOHLGEFAELLE = BUILDING_PROPERTY + "SOHLGEFAELLE"; //$NON-NLS-1$

  public static String BUILDING_PROPERTY_STEIGUNG = BUILDING_PROPERTY + "STEIGUNG"; //$NON-NLS-1$

  public static String BUILDING_PROPERTY_UNTERWASSER = BUILDING_PROPERTY + "UNTERWASSER"; //$NON-NLS-1$

  public static String BUILDING_PROPERTY_WEHRART = BUILDING_PROPERTY + "WEHRART"; //$NON-NLS-1$

  private BuildingsCompatibilityConstants( )
  {
  }
}