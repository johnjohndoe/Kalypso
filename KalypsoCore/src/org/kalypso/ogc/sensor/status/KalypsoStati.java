package org.kalypso.ogc.sensor.status;

/**
 * Kalypso Status Constants
 * 
 * @author schlienger
 */
public interface KalypsoStati
{
  /** Value is OK (0x01) */
  public final static int BIT_OK = 0x01;

  /** Value has to be CHECKed (0x02) */
  public final static int BIT_CHECK = 0x02;

  /** User input is REQUIRED for this value (0x04) */
  public final static int BIT_REQUIRED = 0x04;

  /** Value has been MODIFIED by user (0x08) */
  public final static int BIT_USER_MODIFIED = 0x08;
}
