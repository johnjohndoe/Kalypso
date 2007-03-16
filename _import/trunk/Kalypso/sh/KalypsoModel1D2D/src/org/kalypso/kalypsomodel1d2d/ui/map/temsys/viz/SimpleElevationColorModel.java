/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

//import org.eclipse.swt.graphics.Color;
//import org.eclipse.swt.graphics.Device;
//import org.eclipse.swt.graphics.RGB;

/**
 * @author Patrice Congo
 *
 */
public class SimpleElevationColorModel implements IElevationColorModel
{
  
  public static final double DEEPEST_POINT_ON_EARTH=-10924;
  public static final double HIGHEST_POINT_ON_EARTH=8850;
  
  private double minElevation;
  private double maxElevation;
  private double minBrightness;
  private double maxBrightness;
  private Color noElevationColor;
  private Color baseColor;
  //private RGB rgb;
  private float[] hsb;
  private float[] noElevationColorHSB;
  
  public SimpleElevationColorModel(
            double minElevation,
            double maxElevation,
            Color baseColor,
            double minBrightness,
            double maxBrightness,
            Color noElevationColor)
  {
    this.minElevation=minElevation;
    this.maxElevation = maxElevation;
    this.minBrightness=minBrightness;
    this.maxBrightness=maxBrightness;
    this.noElevationColor=noElevationColor;
    this.baseColor=baseColor;
    this.hsb = Color.RGBtoHSB( baseColor.getRed(),baseColor.getGreen(),baseColor.getBlue(),null);
    this.noElevationColorHSB = Color.RGBtoHSB( noElevationColor.getRed(),
                                               noElevationColor.getGreen(),
                                               noElevationColor.getBlue(),null);
  }
  
  public SimpleElevationColorModel(
    double minElevation1,
    double maxElevation1,
    Color baseColor,
    Color noElevationColor)
  {
    this.minElevation=minElevation1;
    this.maxElevation = maxElevation1;
    this.noElevationColor=noElevationColor;
    this.baseColor=baseColor;
    this.hsb = Color.RGBtoHSB( baseColor.getRed(),baseColor.getGreen(),baseColor.getBlue(),null);
    this.minBrightness = this.hsb[2];
    this.maxBrightness = 0.99;
    this.noElevationColorHSB = Color.RGBtoHSB( noElevationColor.getRed(),
        noElevationColor.getGreen(),
        noElevationColor.getBlue(),null);
  }
  
  public SimpleElevationColorModel( )
  {
//    this(
//        DEEPEST_POINT_ON_EARTH,
//        HIGHEST_POINT_ON_EARTH,
//        Color.BLUE,
//        40,
//        100,
//        Color.RED);
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.ElevationColorModel#getColor(double)
   */
  public Color getColor( double elevation )
  {
    return interpolateColor( elevation );
  }
  
  
  private final Color interpolateColor(double elevation)
  {
    if(Double.isNaN( elevation ))
    {
      return noElevationColor;
    }
    else if(elevation>=minElevation && elevation<=maxElevation)
    {
      double brightness = minBrightness+elevation*(maxBrightness-minBrightness)/(maxElevation-minElevation);
      
      System.out.println("This Color + brightness :"+this.hsb[0]+","+this.hsb[1]+","+brightness);
      System.out.println("This Color :"+hsb[0]+","+hsb[1]+","+hsb[2]);
     // Color.HSBtoRGB( hue, saturation, brightness )
      return Color.getHSBColor( this.hsb[0], this.hsb[1], (float) brightness );
      //new Color( Color.HSBtoRGB( hsb1[0], hsb1[1], hsb1[2] )
      
    }
    else
    {
      throw new IllegalArgumentException(
          "Elevation is out of range:"+
          "\n\tminElevation="+minElevation+
          "\n\tmaxElevation="+maxElevation+
          "\n\tcurrentElevation="+elevation);
    }
  }
  
  public void setElevationMinMax(
                      double minElevation, 
                      double maxElevation)
  {
    this.minElevation=minElevation;
    this.maxElevation=maxElevation;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.IElevationColorModel#getHSB(double)
   */
  public float[] getHSB( double elevation )
  {
    if(Double.isNaN( elevation ))
    {
      return new float[]{noElevationColorHSB[0],
                          noElevationColorHSB[1],
                          noElevationColorHSB[2]};
    }
    else if(elevation>=minElevation && elevation<=maxElevation)
    {
      double brightness = minBrightness+elevation*(maxBrightness-minBrightness)/(maxElevation-minElevation);
      return new float[]{ hsb[0], hsb[1], (float)brightness};
    }
    else
    {
      //or return a translucent color
      throw new IllegalArgumentException(
          "Elevation is out of range:"+
          "\n\tminElevation="+minElevation+
          "\n\tmaxElevation="+maxElevation+
          "\n\tcurrentElevation="+elevation);
    }   
   
  }
  

public float[] getRealRGB(double elevation){
  float[] val = getHSB(elevation);
  return new float[]{
        Color.getHSBColor(  val[0], val[1], val[2] ).getRed(),
        Color.getHSBColor(  val[0], val[1], val[2] ).getGreen(),
        Color.getHSBColor(  val[0], val[1], val[2] ).getBlue()      
  };
  }
  
  
}
