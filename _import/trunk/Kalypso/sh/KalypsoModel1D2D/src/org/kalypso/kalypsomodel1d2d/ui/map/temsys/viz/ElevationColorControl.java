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

/**
 * @author madanago
 *
 */
public class ElevationColorControl
{
  private static final Color DEFAULT_BASE_COLOR = Color.GRAY;
  private static final Color DEFAULT_NO_ELEVATION_COLOR = Color.YELLOW;
  private static final int DEFAULT_COLOR_INDEX = 0;
  
  
  private static Color baseColor = DEFAULT_BASE_COLOR;
  private static Color noElevationColor = DEFAULT_NO_ELEVATION_COLOR;
  
  private static int colorIndex = DEFAULT_COLOR_INDEX;
  
  private static boolean minMaxStatus= false;
  
  public static final IElevationColorModel getColorModel(double minElevation, double maxElevation)
  {
    Color curBaseColor = getBaseColor();
    
    if(curBaseColor==null)
    {
      curBaseColor = DEFAULT_BASE_COLOR;      
    }
    
    Color curNoElevatonColor = getNoElevationColor();
    if(curNoElevatonColor==null)
    {
      curNoElevatonColor = DEFAULT_NO_ELEVATION_COLOR;
    }
    
    return new SimpleElevationColorModel(
                          minElevation, 
                          maxElevation, 
                          curBaseColor,
                          curNoElevatonColor
                          );
  }
  
  public static void setBaseColor( Color baseColor1 )
  {
    ElevationColorControl.baseColor = baseColor1;
  }
  
  public static Color getBaseColor( )
  {
    return baseColor;
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
  
  
  
  
  
}
