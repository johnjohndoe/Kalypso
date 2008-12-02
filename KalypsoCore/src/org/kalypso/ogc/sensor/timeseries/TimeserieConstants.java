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

import org.kalypso.core.i18n.Messages;
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
  public final static String TYPE_RAINFALL = "N"; //$NON-NLS-1$

  /** Abfluss */
  public final static String TYPE_RUNOFF = "Q"; //$NON-NLS-1$

  /** Wasserstand */
  public final static String TYPE_WATERLEVEL = "W"; //$NON-NLS-1$

  /** Normal Null */
  public final static String TYPE_NORMNULL = "NN"; //$NON-NLS-1$

  /** Temperatur */
  public final static String TYPE_TEMPERATURE = "T"; //$NON-NLS-1$

  /** Datum */
  public final static String TYPE_DATE = "date"; //$NON-NLS-1$

  /** Füllung (VOLUMEN) */
  public static final String TYPE_VOLUME = "V"; //$NON-NLS-1$

  /** Evaporation [mm] */
  public static final String TYPE_EVAPORATION = "E"; //$NON-NLS-1$

  /** hours [h] */
  public static final String TYPE_HOURS = "H"; //$NON-NLS-1$

  /** minutes [min] */
  public static final String TYPE_MIN = "min"; //$NON-NLS-1$

  /** area as norm [A/Asum] */
  public static final String TYPE_NORM = "n"; //$NON-NLS-1$

  /** AREA [m^2] */
  public static final String TYPE_AREA = "A"; //$NON-NLS-1$

  /** Die Wurzeltiefe in einem Nutzungszyklus [dm] */
  public static final String TYPE_WT = "WT"; //$NON-NLS-1$

  /** Der Korrekturwert der Verdunstung gegenüber der pot. Verdunstung in einem Nutzungszyklus [-] */
  public static final String TYPE_KC = "KC"; //$NON-NLS-1$

  /** Der Speicherinhalt des Interzeptionsspeichers in einem Nutzungszyklus [mm] */
  public static final String TYPE_LAI = "LAI"; //$NON-NLS-1$

  /** Humidity [%] */
  public static final String TYPE_HUMIDITY = "U"; //$NON-NLS-1$

  /** Velocity [m/s] */
  public static final String TYPE_VELOCITY = "v"; //$NON-NLS-1$

  public static final String TYPE_ORDINAL_NUMBER = "ordinalNr"; //$NON-NLS-1$
  public static final String TYPE_NODEID = "nodeID"; //$NON-NLS-1$
  public static final String TYPE_PEGEL = "pegel"; //$NON-NLS-1$
  
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

  public final static String MD_TIMEZONE = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.17"); //$NON-NLS-1$

  public final static String MD_WQWECHMANN = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.18"); //$NON-NLS-1$

  public final static String MD_WQTABLE = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.19"); //$NON-NLS-1$

  public final static String MD_GKR = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.20"); //$NON-NLS-1$

  public final static String MD_GKH = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.21"); //$NON-NLS-1$

  public final static String MD_COORDSYS = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.22"); //$NON-NLS-1$

  public final static String MD_ALARM_1 = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.23"); //$NON-NLS-1$

  public final static String MD_ALARM_2 = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.24"); //$NON-NLS-1$

  public final static String MD_ALARM_3 = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.25"); //$NON-NLS-1$

  public final static String MD_ALARM_4 = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.26"); //$NON-NLS-1$

  public final static String MD_PEGELNULLPUNKT = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.27"); //$NON-NLS-1$

  public final static String MD_HOEHENANGABEART = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.28"); //$NON-NLS-1$

  public final static String MD_MESSTISCHBLATT = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.29"); //$NON-NLS-1$

  public final static String MD_FLUSSGEBIET = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.30"); //$NON-NLS-1$

  public final static String MD_GEWAESSER = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.31"); //$NON-NLS-1$

  /** Stationskennziffer */
  public final static String MD_KENNZIFFER = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.32"); //$NON-NLS-1$

  /**
   * Markierung für eine Vorhersage. Wenn die Property gesetzt ist (true), handelt es sich um eine Vorhersage Zeitreihe.
   */
  public final static String MD_VORHERSAGE = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.33"); //$NON-NLS-1$

  public final static String MD_DATE_BEGIN = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.34"); //$NON-NLS-1$

  public final static String MD_DATE_END = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.35"); //$NON-NLS-1$

  /** the forecast feature is used in some of the views to mark the forecast date-range */
  public static final String FEATURE_FORECAST = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.36"); //$NON-NLS-1$

  /** the alarm-level feature used to show the alarm-levels in some views */
  public static final String FEATURE_ALARMLEVEL = Messages.getString("org.kalypso.ogc.sensor.timeseries.TimeserieConstants.37"); //$NON-NLS-1$

}