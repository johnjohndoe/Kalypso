package org.kalypso.ogc.sensor.status;

/**
 * Kalypso Status Constants
 * 
 * <b>Hinweise zu den internen Verbrauch von BitMask für den Tagging von Werte
 * (Themengegliedert) </b>:
 * 
 * <pre>
 *     Gültigkeit
 *     0x01 - Für Berechnung ok
 *     0x02 - Für Berechnung eventuell nicht geeignet
 *     0x04 - Für Berechnung nicht geeignet
 * 	   
 *     
 *     Benutzer Eingabe
 *     0x08 - benötigt
 *     0x10 - gesperrt
 *     
 *     Typ
 *     0x20 - gemessene
 *     0x40 - vorhergesagte
 *     
 *     Änderungen vom Benutzer
 *     0x80 - vom Benutzer geändert
 * </pre>
 * 
 * @author schlienger
 */
public interface KalypsoStati
{
  /** Value is OK */
  public final static int BIT_OK = 0x01;

  /** Value is MAYBE ok */
  public final static int BIT_MAYBE = 0x02;

  /** Value is NOT ok */
  public final static int BIT_NOT = 0x04;

  /** User input is REQUIRED for this value */
  public final static int BIT_REQUIRED = 0x08;

  /** User input is LOCKED for this value */
  public final static int BIT_LOCKED = 0x10;

//  /** Value is a MEASURE */
//  public final static int BIT_MEASURE = 0x20;
//
//  /** Value is a FORECAST */
//  public final static int BIT_FORECAST = 0x40;

  /** Value has been MODIFIED by user */
  public final static int BIT_USER_MODIFIED = 0x80;
  
  /** Value is absent, there is a DATA HOLE, probably due to an error in the sensor device */
  public final static int BIT_HOLE = 0x100;
}
