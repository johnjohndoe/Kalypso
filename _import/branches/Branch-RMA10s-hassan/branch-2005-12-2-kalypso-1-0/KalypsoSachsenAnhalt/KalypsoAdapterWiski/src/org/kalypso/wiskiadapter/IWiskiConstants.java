/*--------------- Kalypso-Header ------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 --------------------------------------------------------------------------*/

package org.kalypso.wiskiadapter;

/**
 * 
 * Constants needed for communication with WISKI.
 * 
 * @author Gernot Belger
 */
public interface IWiskiConstants
{
  /** name of the property that delivers the group names */
  final static String PROP_SUPERGROUPNAMES = "SUPERGROUPNAMES";

  /**
   * name of the properties delivering the number of days in the past that can be used as default date-range
   */
  final static String PROP_NUMBER_OF_DAYS = "NUMBER_OF_DAYS";

  final static String MD_WISKI_PARAMETER_TYPE = "Wiski_ParameterType";
  final static String MD_WISKI_PARAMETER_TYPE_LONGNAME = "Wiski_ParametertypeLongname";
  final static String MD_WISKI_STATION_NAME = "Wiski_StationName";
  final static String MD_WISKI_STATION_NO = "Wiski_StationNo";
  final static String MD_WISKI_STATION_PARAMETER_NAME = "Wiski_StationparameterName";
  final static String MD_WISKI_STATION_PARAMETER_LONGNAME = "Wiski_StationparameterLongname";
  final static String MD_WISKI_UNIT = "Wiski_Unit";
  final static String MD_WISKI_WQ_SOURCE = "Wiski_Quelle_Schluesselkurve";

  static final String WISKI_DATA_AXIS_QUALITY = "QUALITY";

  static final Object WISKI_DATA_AXIS_TIMESTAMP = "timestamp";

  static final Object WISKI_DATA_AXIS_VALUE = "tsc_value0";

  static final String WISKI_STATUS_MISSING = "M";

}
