package org.kalypso.ogc.sensor.status;

import java.util.NoSuchElementException;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.impl.DefaultAxis;

/**
 * Utility class for the handling of status information within Kalypso
 * 
 * <p>
 * <b>Hinweise zu den internen Verbrauch von BitMask für den Tagging von Werte
 * (Themengegliedert) </b>:
 * 
 * <pre>
 *     Gültigkeit
 *     0x01 - Für Berechnung ok
 *     0x02 - Für Berechnung eventuell nicht geeignet
 *     0x04 - Für Berechnung nicht geeignet
 *     
 *     Benutzer Eingabe
 *     0x08 - benötigt
 *     0x10 - gesperrt
 *     
 *     Typ
 *     0x12 - gemessene
 *     0x14 - vorhergesagte
 *     
 *     Änderungen vom Benutzer
 *     0x1F - vom Benutzer geändert
 * </pre>
 * 
 * @author schlienger
 */
public class KalypsoStatusUtils
{
  private final static String STATUS_AXIS_LABELPREFIX = "_kalypso_status_";

  public final static String STATUS_AXIS_TYPE = "kalypso-status";

  public final static Class STATUS_AXIS_DATACLASS = Integer.class;

  public final static String STATUS_AXIS_UNIT = "";

  public final static int BIT_OK = 0x01;

  public final static int BIT_MAYBE = 0x02;

  public final static int BIT_NOT = 0x04;

  public final static int BIT_REQUIRED = 0x08;

  public final static int BIT_LOCKED = 0x10;

  public final static int BIT_MEASURE = 0x12;

  public final static int BIT_FORECAST = 0x14;

  public final static int BIT_USER_MODIFIED = 0x1F;

  private KalypsoStatusUtils( )
  {
    // not to be instanciated
  }

  /**
   * Returns the status axis name for the given value axis.
   * 
   * @param axis
   *          the observation axis for which to return the status axis name
   * @return the name of the corresponding status axis
   */
  public static String getStatusAxisLabelFor( final IAxis axis )
  {
    if( isStatusAxis( axis ) )
      return axis.getName();

    return STATUS_AXIS_LABELPREFIX + axis.getName();
  }

  /**
   * Creates a status axis for the given 'normal' axis.
   * 
   * @param axis
   * @param pos
   * @return new status axis
   * @throws IllegalArgumentException
   *           if given axis is already a status axis
   */
  public static IAxis getStatusAxisFor( final IAxis axis, final int pos )
      throws IllegalArgumentException
  {
    if( isStatusAxis( axis ) )
      throw new IllegalArgumentException( "Axis " + axis
          + " is already a status axis!" );

    return new DefaultAxis( STATUS_AXIS_LABELPREFIX + axis.getName(),
        STATUS_AXIS_TYPE, STATUS_AXIS_UNIT, STATUS_AXIS_DATACLASS, pos, false );
  }

  /**
   * Returns true if the axis is a status-axis (speaking Kalypso intern)
   * 
   * @param axis
   * @return true if status-axis
   */
  public static boolean isStatusAxis( final IAxis axis )
  {
    return axis.getType().equals( STATUS_AXIS_TYPE );
  }

  /**
   * Finds the first status axis among the given list.
   * 
   * @param axes
   * @return status axis
   * @throws NoSuchElementException
   *           if no status axis in the list
   */
  public static IAxis findStatusAxis( final IAxis[] axes )
      throws NoSuchElementException
  {
    for( int i = 0; i < axes.length; i++ )
    {
      if( isStatusAxis( axes[i] ) )
        return axes[i];
    }

    throw new NoSuchElementException( "No Status-Axis found" );
  }

  /**
   * Checks if bit is in the mask.
   * 
   * @param mask
   * @param bit
   * @return true if bit is set in the given mask
   */
  public static boolean checkMask( final int mask, final int bit )
  {
    return (mask & bit) == bit;
  }
}