package org.kalypso.ogc.sensor.timeseries;

/**
 * Constants used within the sensor package.
 * 
 * @author schlienger
 */
public interface TimeserieConstants
{
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
}
