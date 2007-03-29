/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz;

import java.awt.Color;

import org.eclipse.jface.preference.IPreferenceStore;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.ColorModelChangeComponent;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.IColorModelPreferenceConstants;

/**
 * @author madanago
 */
public class ElevationColorControl implements IColorModelPreferenceConstants
{
  static private IPreferenceStore preferenceStore_ = KalypsoModel1D2DPlugin.getDefault().getPreferenceStore();

  private static final Color DEFAULT_MIN_COLOR = Color.RED;

  private static final Color DEFAULT_MAX_COLOR = Color.GREEN;

  private static final Color DEFAULT_NO_ELEVATION_COLOR = Color.YELLOW;

  private static final int DEFAULT_COLOR_INDEX = 10;

  private static final int DEFAULT_TRANSPARENCY_INDEX = 50;

  private static final boolean DEFAULT_MINMAX = false;

  private static Color m_baseColor = null;

  private static Color m_minColor = null;

  private static Color m_maxColor = null;

  private static Color noElevationColor = DEFAULT_NO_ELEVATION_COLOR;

  private static int colorIndex = DEFAULT_COLOR_INDEX;

  private static int transparencyIndex = DEFAULT_TRANSPARENCY_INDEX;

  private static boolean minMaxStatus = DEFAULT_MINMAX;

  private static double m_maxElevation;

  private static double m_minElevation;

  /* int colors from preferency store */
  static
  {
    if( !preferenceStore_.contains( LINE_COLOR_INDEX ) )
    {
      colorIndex = DEFAULT_COLOR_INDEX;
    }
    else
      colorIndex = preferenceStore_.getInt( LINE_COLOR_INDEX );

    if( !preferenceStore_.contains( LINE_TRANSPARENCY ) )
    {
      transparencyIndex = DEFAULT_TRANSPARENCY_INDEX;
    }
    else
      transparencyIndex = preferenceStore_.getInt( LINE_TRANSPARENCY );

    if( !preferenceStore_.contains( LINE_MIN_MAX ) )
    {
      minMaxStatus = DEFAULT_MINMAX;
    }
    else
      minMaxStatus = preferenceStore_.getBoolean( LINE_MIN_MAX );

    if( !preferenceStore_.contains( LINE_MAX_COLOR ) )
    {
      m_maxColor = DEFAULT_MAX_COLOR;
    }
    else
      m_maxColor = ColorModelChangeComponent.getThisColor( LINE_MAX_COLOR );

    if( !preferenceStore_.contains( LINE_MIN_COLOR ) )
    {
      m_minColor = DEFAULT_MIN_COLOR;
    }
    else
      m_minColor = ColorModelChangeComponent.getThisColor( LINE_MIN_COLOR );

    if( !preferenceStore_.contains( LINE_NO_COLOR ) )
    {
      noElevationColor = DEFAULT_NO_ELEVATION_COLOR;
    }
    else
      noElevationColor = ColorModelChangeComponent.getThisColor( LINE_NO_COLOR );

  }

  public static final IElevationColorModel getColorModel( double minElevation, double maxElevation )
  {
    return new SimpleElevationColorModel( minElevation, maxElevation, getMinColor(), getMaxColor(), getNoElevationColor(), getTransparencyIndex(), getColorIndex(), getMinMaxStatus() );
  }

  public static final IElevationColorModel getColorModel( )
  {
    return new SimpleElevationColorModel( m_minElevation, m_maxElevation, getMinColor(), getMaxColor(), getNoElevationColor(), getTransparencyIndex(), getColorIndex(), getMinMaxStatus() );
  }
  
  public static void setBaseColor( Color baseColor1 )
  {
    ElevationColorControl.m_baseColor = baseColor1;
  }

  public static void setMinColor( Color minColor )
  {
    m_minColor = minColor;
  }

  public static void setMaxColor( Color maxColor )
  {
    m_maxColor = maxColor;
  }

  public static Color getBaseColor( )
  {
    return m_baseColor;
  }

  public static Color getMaxColor( )
  {
    return m_maxColor;
  }

  public static Color getMinColor( )
  {
    return m_minColor;
  }

  public static Color getNoElevationColor( )
  {
    return noElevationColor;
  }

  public static void setNoElevationColor( Color noElevationColor1 )
  {
    ElevationColorControl.noElevationColor = noElevationColor1;
  }

  public static int getColorIndex( )
  {

    return colorIndex;
  }

  public static void setColorIndex( int colorIndex1 )
  {
    ElevationColorControl.colorIndex = colorIndex1;
  }

  public static boolean getMinMaxStatus( )
  {
    return minMaxStatus;
  }

  public static void setMinMaxStatus( boolean minMaxStatus1 )
  {
    ElevationColorControl.minMaxStatus = minMaxStatus1;
  }

  public static int getTransparencyIndex( )
  {

    return transparencyIndex;
  }

  public static void setTransparencyIndex( int transparencyIndex1 )
  {
    ElevationColorControl.transparencyIndex = transparencyIndex1;
  }
  
  public static void setMaxElevation( double max )
  {
    m_maxElevation = max;
  }
  
  public static void setMinElevation( double min )
  {
    m_minElevation = min;
  }
  
  public static double getMaxElevation(  )
  {
    return m_maxElevation;
  }
  
  public static double getMinElevation( )
  {
    return m_minElevation;
  }
  
}
