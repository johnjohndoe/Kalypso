package org.kalypso.ogc.sensor.status;

import java.util.ArrayList;
import java.util.NoSuchElementException;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.impl.DefaultAxis;

/**
 * Utility class for the handling of status information within Kalypso
 * 
 * 
 * @author schlienger
 */
public class KalypsoStatusUtils
{
  private final static String STATUS_AXIS_LABELPREFIX = "_kalypso_status_";

  public final static String STATUS_AXIS_TYPE = "kalypso-status";

  public final static Class STATUS_AXIS_DATACLASS = Integer.class;

  public final static String STATUS_AXIS_UNIT = "";

  private final static Icon ICON_QUESTION = new ImageIcon(
      KalypsoStatusUtils.class.getResource( "resource/question.gif" ) );

  private final static Icon ICON_WARNING = new ImageIcon(
      KalypsoStatusUtils.class.getResource( "resource/warning.gif" ) );

  private final static Icon ICON_ERROR = new ImageIcon(
      KalypsoStatusUtils.class.getResource( "resource/error.gif" ) );

  private final static Icon ICON_WRITE = new ImageIcon(
      KalypsoStatusUtils.class.getResource( "resource/write.gif" ) );

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
   * Returns the axis label without the status marker
   * 
   * @param axis
   * @return just axis label
   */
  public static String getAxisLabelFor( final IAxis axis )
  {
    return axis.getName().replaceAll( STATUS_AXIS_LABELPREFIX, "" );
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
  public static IAxis createStatusAxisFor( final IAxis axis, final int pos )
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
   * Finds the list of status axis among the given axes
   * 
   * @param axes
   * @return status axes
   */
  public static IAxis[] findStatusAxes( final IAxis[] axes )
  {
    final ArrayList list = new ArrayList();

    for( int i = 0; i < axes.length; i++ )
    {
      if( isStatusAxis( axes[i] ) )
        list.add( axes[i] );
    }

    return (IAxis[]) list.toArray( new IAxis[list.size()] );
  }

  /**
   * Returns the list of non-status axes
   * 
   * @param axes
   * @return non-status axes
   */
  public static IAxis[] withoutStatusAxes( final IAxis[] axes )
  {
    final ArrayList list = new ArrayList();

    for( int i = 0; i < axes.length; i++ )
    {
      if( !isStatusAxis( axes[i] ) )
        list.add( axes[i] );
    }

    return (IAxis[]) list.toArray( new IAxis[list.size()] );
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

  /**
   * @param mask
   * @return the icon that best fits the mask, or null if no fit
   */
  public static Icon getIconFor( final int mask )
  {
    if( checkMask( mask, KalypsoStati.BIT_CHECK ) )
      return ICON_WARNING;

    if( checkMask( mask, KalypsoStati.BIT_REQUIRED ) )
      return ICON_QUESTION;

    if( checkMask( mask, KalypsoStati.BIT_USER_MODIFIED ) )
      return ICON_WRITE;

    return null;
  }

  /**
   * @param mask
   * @return the tooltip that best fits the mask, or null if no fit
   */
  public static String getTooltipFor( final int mask )
  {
    if( checkMask( mask, KalypsoStati.BIT_CHECK ) )
      return "Wert auf Gültigkeit prüfen";

    if( checkMask( mask, KalypsoStati.BIT_REQUIRED ) )
      return "Eingabe erforderlich";

    if( checkMask( mask, KalypsoStati.BIT_USER_MODIFIED ) )
      return "Vom Benutzer geändert";

    return null;
  }
}