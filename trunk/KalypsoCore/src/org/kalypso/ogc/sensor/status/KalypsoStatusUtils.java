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
      KalypsoStatusUtils.class.getResource( "resource/question.gif" ), "question" );

  private final static ImageIcon ICON_WARNING = new ImageIcon(
      KalypsoStatusUtils.class.getResource( "resource/warning.gif" ), "warning" );

//  private final static Icon ICON_ERROR = new ImageIcon(
//      KalypsoStatusUtils.class.getResource( "resource/error.gif" ) );

  private final static ImageIcon ICON_WRITE = new ImageIcon(
      KalypsoStatusUtils.class.getResource( "resource/write.gif" ), "write" );

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
   * @return new status axis
   * @throws IllegalArgumentException
   *           if given axis is already a status axis
   */
  public static IAxis createStatusAxisFor( final IAxis axis )
      throws IllegalArgumentException
  {
    if( isStatusAxis( axis ) )
      throw new IllegalArgumentException( "Axis " + axis
          + " is already a status axis!" );

    return new DefaultAxis( STATUS_AXIS_LABELPREFIX + axis.getName(),
        STATUS_AXIS_TYPE, STATUS_AXIS_UNIT, STATUS_AXIS_DATACLASS, false );
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
  public static ImageIcon getIconFor( final int mask )
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
   * Returns an icon that corresponds to the given description. Description can be
   * any of the following:
   * <ol>
   * <li>"question": the Question Icon
   * <li>"warning": the Warning Icon
   * <li>"write": the Write Icon
   * <li>any URL: an URL that will be used for finding the image (see ImageIcon.ImageIcon( URL ) )
   * <li>null: returns null
   * </ol>
   * 
   * @param iconDescription
   * @return icon
   * @see KalypsoStatusUtils#ICON_QUESTION
   * @see KalypsoStatusUtils#ICON_WARNING
   * @see KalypsoStatusUtils#ICON_WRITE
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

    try
    {
      return new ImageIcon( new URL( iconDescription ) );
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
      return null;
    }
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