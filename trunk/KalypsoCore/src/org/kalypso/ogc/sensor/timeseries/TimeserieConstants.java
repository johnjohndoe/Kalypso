package org.kalypso.ogc.sensor.timeseries;

import java.text.DateFormat;

import org.kalypso.ogc.sensor.ObservationConstants;

/**
 * Constants used within the sensor package.
 * 
 * @author schlienger
 */
public abstract class TimeserieConstants implements ObservationConstants
{
  /** default date format used within some of the timeseries dependent properties */
  public final static DateFormat DEFAULT_DF = DateFormat.getDateTimeInstance();
  
  /** Niederschlag */
  public final static String TYPE_RAINFALL = "N";
  
  /** Abfluss */
  public final static String TYPE_RUNOFF = "Q";
  
  /** Wasserstand */
  public final static String TYPE_WATERLEVEL = "W";
  
  /** Temperatur */
  public final static String TYPE_TEMPERATURE = "T";
  
  /** Datum */
  public final static String TYPE_DATE = "date";
  
  /** Füllung (VOLUMEN) */
  public static final String TYPE_VOLUME = "V";
  
  public final static String MD_WQ = "WQ-Parameter";

  public final static String MD_GKR = "Rechtswert";

  public final static String MD_GKH = "Hochwert";

  public final static String MD_ALARM_1 = "Alarmstufe 1";

  public final static String MD_ALARM_2 = "Alarmstufe 2";

  public final static String MD_ALARM_3 = "Alarmstufe 3";

  public final static String MD_ALARM_4 = "Alarmstufe 4";

  public final static String MD_PEGELNULLPUNKT = "Pegelnullpunkt";

  public final static String MD_HOEHENANGABEART = "Höhenangabeart";

  public final static String MD_MESSTISCHBLATT = "Messtischblattnummer";
  
  public final static String MD_FLUSSGEBIET = "Flussgebiet";
  
  public final static String MD_FLUSS = "Fluss";

  /**
   * Markierung für eine Vorhersage. Wenn die Property gesetzt ist (true),
   * handelt es sich um eine Vorhersage Zeitreihe.
   */
  public final static String MD_VORHERSAGE = "Vorhersage";
  
  /**
   * @param type
   * @return corresponding unit
   */
  public static String getUnit( final String type )
  {
    // TODO!
    
    return type;
  }
  
  /**
   * @param type
   * @return corresponding name (user friendly)
   */
  public static String getName( final String type )
  {
    // TODO!
    
    return type;
  }
}
