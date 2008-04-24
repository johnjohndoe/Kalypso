/*--------------- Kalypso-Header --------------------------------------------------------------------

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
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.timeseries;

import java.text.DateFormat;
import java.util.Arrays;

import org.kalypso.ogc.sensor.ObservationConstants;

/**
 * Constants used within the sensor package.
 * 
 * @author schlienger
 */
public abstract class TimeserieConstants implements ObservationConstants
{
  /** default date format used within some of the timeseries dependent properties */
// TODO: this dateFormat depends on the locale. This results in problems reading ZML files created using a different
// locale
  public final static DateFormat DEFAULT_DF = DateFormat.getDateTimeInstance();

  /** Niederschlag */
  public final static String TYPE_RAINFALL = "N";

  /** Abfluss */
  public final static String TYPE_RUNOFF = "Q";

  /** Wasserstand */
  public final static String TYPE_WATERLEVEL = "W";

  /** Normal Null */
  public final static String TYPE_NORMNULL = "NN";

  /** Temperatur */
  public final static String TYPE_TEMPERATURE = "T";

  /** Datum */
  public final static String TYPE_DATE = "date";

  /** Füllung (VOLUMEN) */
  public static final String TYPE_VOLUME = "V";

  /** Evaporation [mm] */
  public static final String TYPE_EVAPORATION = "E";

  /** hours [h] */
  public static final String TYPE_HOURS = "H";

  /** minutes [min] */
  public static final String TYPE_MIN = "min";

  /** area as norm [A/Asum] */
  public static final String TYPE_NORM = "n";

  /** AREA [m^2] */
  public static final String TYPE_AREA = "A";

  /** Die Wurzeltiefe in einem Nutzungszyklus [dm] */
  public static final String TYPE_WT = "WT";

  /** Der Korrekturwert der Verdunstung gegenüber der pot. Verdunstung in einem Nutzungszyklus [-] */
  public static final String TYPE_KC = "KC";

  /** Der Speicherinhalt des Interzeptionsspeichers in einem Nutzungszyklus [mm] */
  public static final String TYPE_LAI = "LAI";

  /** Humidity [%] */
  public static final String TYPE_HUMIDITY = "U";

  /** Velocity [m/s] */
  public static final String TYPE_VELOCITY = "v";

  public static final String[] TYPES_ALL;

  /**
   * to enable seaching in types the array must be sorted
   */
  static
  {
    String[] types = new String[] { TYPE_DATE, TYPE_EVAPORATION, TYPE_RAINFALL, TYPE_RUNOFF, TYPE_TEMPERATURE, TYPE_VOLUME, TYPE_WATERLEVEL, TYPE_NORM, TYPE_AREA, TYPE_HOURS, TYPE_NORMNULL, TYPE_KC,
        TYPE_WT, TYPE_LAI, TYPE_HUMIDITY, TYPE_VELOCITY };
    Arrays.sort( types );
    TYPES_ALL = types;
  }

  // METADATEN

  public final static String MD_TIMEZONE = "Zeitzone";

  public final static String MD_WQWECHMANN = "WQ-Parameter";

  public final static String MD_WQTABLE = "WQ-Tabelle";

  public final static String MD_GKR = "Rechtswert";

  public final static String MD_GKH = "Hochwert";

  public final static String MD_COORDSYS = "Koordinatensystem";

  public final static String MD_ALARM_1 = "Alarmstufe 1";

  public final static String MD_ALARM_2 = "Alarmstufe 2";

  public final static String MD_ALARM_3 = "Alarmstufe 3";

  public final static String MD_ALARM_4 = "Alarmstufe 4";

  public final static String MD_PEGELNULLPUNKT = "Pegelnullpunkt";

  public final static String MD_HOEHENANGABEART = "Höhenangabeart";

  public final static String MD_MESSTISCHBLATT = "Messtischblattnummer";

  public final static String MD_FLUSSGEBIET = "Flussgebiet";

  public final static String MD_GEWAESSER = "Gewässer";

  /** Stationskennziffer */
  public final static String MD_KENNZIFFER = "Kennziffer";

  /**
   * Markierung für eine Vorhersage. Wenn die Property gesetzt ist (true), handelt es sich um eine Vorhersage Zeitreihe.
   */
  public final static String MD_VORHERSAGE = "Vorhersage";

  public final static String MD_DATE_BEGIN = "Datum-Von";

  public final static String MD_DATE_END = "Datum-Bis";

  /** the forecast feature is used in some of the views to mark the forecast date-range */
  public static final String FEATURE_FORECAST = "Vorhersage";

  /** the alarm-level feature used to show the alarm-levels in some views */
  public static final String FEATURE_ALARMLEVEL = "Alarmstufen";

}