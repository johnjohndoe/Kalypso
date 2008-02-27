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
package org.kalypso.preferences;

/**
 * Preference constants.
 * 
 * @author Holger Albert
 */
public interface IKalypsoDeegreePreferences
{
  /**
   * Defines the key for available coordinate systems.
   */
  public static final String AVAILABLE_CRS_SETTING = "org.kalypso.deegree.coordinate.systems";

  /**
   * Defines the default values for available coordinate systems.
   */
  public static final String AVAILABLE_CRS_VALUE = "EPSG:3034;EPSG:3035;EPSG:4258;EPSG:4289;EPSG:4314;EPSG:4326;EPSG:31466;EPSG:31467;EPSG:31468;EPSG:31469;EPSG:25831;EPSG:25832;EPSG:25833;EPSG:32631;EPSG:32632;EPSG:32633";
}