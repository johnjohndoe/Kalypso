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
package org.kalypso.ogc.sensor.status;

import java.awt.Color;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.NoSuchElementException;

import javax.swing.ImageIcon;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.impl.DefaultAxis;

/**
 * Utility class for the handling of status information within Kalypso
 * 
 * @author schlienger
 */
public class KalypsoStatusUtils
{
  private final static String STATUS_AXIS_LABELPREFIX = "_kalypso_status_";

  public final static String STATUS_AXIS_TYPE = "kalypso-status";

  public final static Class STATUS_AXIS_DATACLASS = Integer.class;

  public final static String STATUS_AXIS_UNIT = "";

  private final static ImageIcon ICON_QUESTION = new ImageIcon(
      KalypsoStatusUtils.class.getResource( "resource/question.gif" ),
      "question" );

  private final static ImageIcon ICON_WARNING = new ImageIcon(
      KalypsoStatusUtils.class.getResource( "resource/warning.gif" ), "warning" );

  //  private final static Icon ICON_ERROR = new ImageIcon(
  //      KalypsoStatusUtils.class.getResource( "resource/error.gif" ) );

  private final static ImageIcon ICON_CONFLICT = new ImageIcon(
      KalypsoStatusUtils.class.getResource( "resource/conflict.gif" ) );

  private final static ImageIcon ICON_WRITE = new ImageIcon(
      KalypsoStatusUtils.class.getResource( "resource/write.gif" ), "write" );

  private final static Color COLOR_LIGHTYELLOW = new Color( 248, 243, 192);
  
  private KalypsoStatusUtils()
  {
    // not to be instanciated
  }

  /**
   * Returns the status axis name for the given value axis.
   * 
   * @param axis the observation axis for which to return the status axis name
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
   * @return just axis label
   */
  public static String getAxisLabelFor( final IAxis axis )
  {
    return axis.getName().replaceAll( STATUS_AXIS_LABELPREFIX, "" );
  }

  /**
   * Creates a status axis for the given 'normal' axis.
   * 
   * @return new status axis
   * @throws IllegalArgumentException if given axis is already a status axis
   */
  public static IAxis createStatusAxisFor( final IAxis axis,
      final boolean persistable ) throws IllegalArgumentException
  {
    if( isStatusAxis( axis ) )
      throw new IllegalArgumentException( "Axis " + axis
          + " is already a status axis!" );

    return new DefaultAxis( STATUS_AXIS_LABELPREFIX + axis.getName(),
        STATUS_AXIS_TYPE, STATUS_AXIS_UNIT, STATUS_AXIS_DATACLASS, false,
        persistable );
  }

  /**
   * Returns true if the axis is a status-axis (speaking Kalypso intern)
   * 
   * @return true if status-axis
   */
  public static boolean isStatusAxis( final IAxis axis )
  {
    return axis.getType().equals( STATUS_AXIS_TYPE );
  }

  /**
   * Returns true if the given statusCandidate is the status axis for the given
   * axis
   */
  public static boolean isStatusAxisFor( final IAxis axis,
      final IAxis statusCandidate )
  {
    final String statusAxisLabel = getStatusAxisLabelFor( axis );

    return statusCandidate.getName().equals( statusAxisLabel );
  }

  /**
   * Return true if both axes are equal in the sense of IAxis.equals() plus:
   * <ul>
   * <li>both axes are status-axes
   * <li>both axes have the same name
   * </ul>
   */
  public static boolean equals( final IAxis axis1, final IAxis axis2 )
  {
    if( !axis1.equals( axis2 ) )
      return false;

    return isStatusAxis( axis1 ) && isStatusAxis( axis2 )
        && axis1.getName().equals( axis2.getName() );
  }

  /**
   * Finds the first status axis among the given list.
   * 
   * @return status axis
   * @throws NoSuchElementException if no status axis in the list
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
   * Returns the status axis for the given axis if found in the axes-list.
   * 
   * @throws NoSuchElementException if no corresponding status axis found
   */
  public static IAxis findStatusAxisFor( final IAxis[] axes, final IAxis axis )
      throws NoSuchElementException
  {
    for( int i = 0; i < axes.length; i++ )
    {
      if( isStatusAxisFor( axis, axes[i] ) )
        return axes[i];
    }

    throw new NoSuchElementException( "No Status-Axis found for: " + axis );
  }

  /**
   * Finds the list of status axis among the given axes
   * 
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

    return (IAxis[])list.toArray( new IAxis[list.size()] );
  }

  /**
   * Returns the list of non-status axes
   * 
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

    return (IAxis[])list.toArray( new IAxis[list.size()] );
  }

  /**
   * Returns the axes that are compatible with the desired Dataclass. You can
   * specify if you want to exclude the status axes from the result list or not.
   * <p>
   * Please note that currently the status axis is of a Number type.
   * 
   * @param axes
   * @param desired
   * @param excludeStatusAxes if true, status axes will not be included in the
   *          returned array
   * @return axes which are compatible with specified Class of data
   * @throws NoSuchElementException
   */
  public static IAxis[] findAxesByClass( final IAxis[] axes,
      final Class desired, final boolean excludeStatusAxes )
      throws NoSuchElementException
  {
    final ArrayList list = new ArrayList( axes == null ? 0 : axes.length );

    for( int i = 0; i < axes.length; i++ )
    {
      if( desired.isAssignableFrom( axes[i].getDataClass() ) )
      {
        if( !excludeStatusAxes || excludeStatusAxes
            && !KalypsoStatusUtils.isStatusAxis( axes[i] ) )
          list.add( axes[i] );
      }
    }

    if( list.size() == 0 )
      throw new NoSuchElementException( "No axis found of class: " + desired );

    return (IAxis[])list.toArray( new IAxis[list.size()] );
  }

  /**
   * Returns the first axis that is compatible with the desired Dataclass. You
   * can specify if you want to exclude the status axes from the result list or
   * not.
   * <p>
   * Please note that currently the status axis is of a Number type.
   * 
   * @param axes
   * @param desired
   * @param excludeStatusAxes if true, status axes will not be included in the
   *          returned array
   * @return first axis found
   * @throws NoSuchElementException
   */
  public static IAxis findAxisByClass( final IAxis[] axes, final Class desired,
      final boolean excludeStatusAxes ) throws NoSuchElementException
  {
    for( int i = 0; i < axes.length; i++ )
    {
      if( desired.isAssignableFrom( axes[i].getDataClass() ) )
      {
        if( !excludeStatusAxes || excludeStatusAxes
            && !KalypsoStatusUtils.isStatusAxis( axes[i] ) )
          return axes[i];
      }
    }

    throw new NoSuchElementException( "No Axis found of class: " + desired );
  }

  /**
   * Checks if bit is in the mask.
   * 
   * @return true if bit is set in the given mask
   */
  public static boolean checkMask( final int mask, final int bit )
  {
    return ( mask & bit ) == bit;
  }

  /**
   * @return the icon that best fits the mask, or null if no fit
   */
  public static ImageIcon getIconFor( final int mask )
  {
    if( checkMask( mask, KalypsoStati.BIT_CHECK ) )
      return ICON_WARNING;

    if( checkMask( mask, KalypsoStati.BIT_REQUIRED ) )
      return ICON_QUESTION;

    if( checkMask( mask, KalypsoStati.BIT_USER_MODIFIED ) )
      return ICON_WRITE;

    if( checkMask( mask, KalypsoStati.BIT_DERIVATION_ERROR ) )
      return ICON_CONFLICT;

    return null;
  }

  /**
   * Returns an icon that corresponds to the given description. Description can
   * be any of the following:
   * <ol>
   * <li>"question": the Question Icon
   * <li>"warning": the Warning Icon
   * <li>"write": the Write Icon
   * <li>"conflict": the Conflict Icon
   * <li>any URL: an URL that will be used for finding the image (see
   * ImageIcon.ImageIcon( URL ) )
   * <li>null: returns null
   * </ol>
   * 
   * @see KalypsoStatusUtils#ICON_QUESTION
   * @see KalypsoStatusUtils#ICON_WARNING
   * @see KalypsoStatusUtils#ICON_WRITE
   * @see KalypsoStatusUtils#ICON_CONFLICT
   * @see ImageIcon#ImageIcon(java.net.URL)
   */
  public static ImageIcon getIconFor( final String iconDescription )
  {
    if( iconDescription == null )
      return null;

    if( "question".equalsIgnoreCase( iconDescription ) )
      return ICON_QUESTION;
    if( "warning".equalsIgnoreCase( iconDescription ) )
      return ICON_WARNING;
    if( "write".equalsIgnoreCase( iconDescription ) )
      return ICON_WRITE;
    if( "conflict".equalsIgnoreCase( iconDescription ) )
      return ICON_CONFLICT;

    try
    {
      return new ImageIcon( new URL( iconDescription ) );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /**
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

    if( checkMask( mask, KalypsoStati.BIT_DERIVATION_ERROR ) )
      return "Wert konnte nicht abgeleitet werden";

    if( checkMask( mask, KalypsoStati.BIT_DERIVATED ) )
      return "Wert wurde abgeleitet";

    return null;
  }
  
  /**
   * @return mask dependent color to be used as foreground
   */
  public static Color getForegroundFor( final int mask )
  {
    // currently returns null, but can be customized in the near
    // future
    return null;
  }
  
  /**
   * @return mask dependent color to be used as background
   */
  public static Color getBackgroundFor( final int mask )
  {
    if( checkMask( mask, KalypsoStati.BIT_DERIVATION_ERROR ) )
      return COLOR_LIGHTYELLOW;

    if( checkMask( mask, KalypsoStati.BIT_DERIVATED ) )
      return COLOR_LIGHTYELLOW;

    // customisation possible in the near future...
    return null;
  }
}