package org.kalypso.ogc.sensor.status;

import org.kalypso.ogc.sensor.IAxis;

/**
 * Utility class for the handling of status information within Kalypso
 * 
 * <p>
 * <b>Hinweise zu den internen Verbrauch von BitMask für den Tagging von Werte
 * (Themengegliedert) </b>:
 * 
 * <pre>
 * <b>Gültigkeit</b>
 * 0x01 - Für Berechnung ok
 * 0x02 - Für Berechnung eventuell nicht geeignet
 * 0x04 - Für Berechnung nicht geeignet
 * 
 * <b>Benutzer Eingabe</b>
 * 0x08 - benötigt
 * 0x0F - optional
 * 0x10 - gesperrt
 * 
 * <b>Typ</b>
 * 0x12 - gemessene
 * 0x14 - vorhergesagte
 * 
 * <b>Änderungen vom Benutzer</b>
 * 0x18 - vom Benutzer nicht geändert
 * 0x1F - vom Benutzer geändert
 * </pre>
 * 
 * @author schlienger
 */
public class KalypsoStatusUtils
{
  private final static String STATUS_AXIS_LABEL = "_kalypso_status_";
  
  public final static String STATUS_AXIS_DATATYPE = "TYPE=xs:integer";
  public final static String STATUS_AXIS_UNIT = "";
  public final static String STATUS_AXIS_SEPARATOR = " ";
  public final static String STATUS_AXIS_VALUES = "";

  private KalypsoStatusUtils()
  {
  // not to be instanciated
  }

  /**
   * Builds the kalypso internal status axis name for that value axis
   * 
   * @param axis
   *          the observation axis for which to build the status axis name
   * @return the name of the corresponding status axis
   * @throws IllegalArgumentException
   *           if axis is alreay a status-axis
   */
  public static String getStatusAxisLabelFor( final IAxis axis ) throws IllegalArgumentException
  {
    if( isStatusAxis( axis ) )
      throw new IllegalArgumentException( "Axis " + axis + " is already a status axis!" );

    return STATUS_AXIS_LABEL + axis.getLabel();
  }

  /**
   * Returns true if the axis is a status-axis (speaking Kalypso intern)
   * 
   * @param axis
   * @return true if status-axis
   */
  public static boolean isStatusAxis( final IAxis axis )
  {
    return axis.getLabel().startsWith( STATUS_AXIS_LABEL );
  }
}
