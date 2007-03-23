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
  
  private double m_minElevation;
  private double m_maxElevation;
  private double m_minHue;
  private double m_maxHue;
  
  private double m_minSat;
  private double m_maxSat;
  
  private double minBri;
  private double maxBri;
  
  private Color noElevationColor;
  private Color baseColor;
  private float alfa = 0.5f;
  
  //private RGB rgb;
  private float[] m_hsb;
  private float[] noElevationColorHSB;
  private int m_transparency;
  private boolean m_goDarkerFromMinToMax;
  private Color m_minColor;
  private Color m_maxColor;
  private float[] m_minhsb;
  private float[] m_maxhsb;
  private double m_minBri;
  private double m_maxBri;
  private double m_elevationClassRange;
  private double m_classMinElevation;
  private double m_classMaxElevation;
  
  public SimpleElevationColorModel(
    double minElevation,
    double maxElevation,
    Color minColor,
    Color maxColor,
    Color noElevationColor,
    double transparency,
    int numOfClasses,
    boolean goDarkerFromMinToMax)
  {
    m_minElevation = minElevation;
    m_maxElevation = maxElevation;
    this.noElevationColor=noElevationColor;
    m_minColor=minColor;
    m_maxColor=maxColor;
    m_minhsb = Color.RGBtoHSB( m_minColor.getRed(),m_minColor.getGreen(),m_minColor.getBlue(),null);
    m_maxhsb = Color.RGBtoHSB( m_maxColor.getRed(),m_maxColor.getGreen(),m_maxColor.getBlue(),null);
  /*  this.minBrightness = this.hsb[0];//2
    this.maxBrightness = 1.00;*/
    
    m_elevationClassRange = (m_maxElevation - m_minElevation) / numOfClasses;
    
    m_classMinElevation = m_minElevation + m_elevationClassRange / 2;
    m_classMaxElevation = m_maxElevation - m_elevationClassRange / 2;
    
    m_minHue = m_minhsb[0];
    m_maxHue = m_maxhsb[0];
    
    m_minSat = m_minhsb[1];
    m_maxSat = m_maxhsb[1];
    
    m_minBri = m_minhsb[2];
    m_maxBri = m_maxhsb[2];
    
    this.noElevationColorHSB = Color.RGBtoHSB( noElevationColor.getRed(),
        noElevationColor.getGreen(),
        noElevationColor.getBlue(),null);
    m_transparency = (255 -(int)(transparency*255.0/100.0));
    m_goDarkerFromMinToMax = goDarkerFromMinToMax;
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
    else if(elevation>=m_minElevation && elevation<=m_maxElevation)
    {      
      double interpolSat;
      double interpolHue;
      double interpolBri;
      
      
      if (!m_goDarkerFromMinToMax)
      {
        interpolHue = m_minHue+(elevation*(m_maxHue-m_minHue)/(m_classMaxElevation - m_classMinElevation));
        interpolSat = m_minSat+(elevation*(m_maxSat-m_minSat)/(m_classMaxElevation - m_classMinElevation));      
        interpolBri = m_minBri+(elevation*(m_maxBri-m_minBri)/(m_classMaxElevation - m_classMinElevation));
      }
      else
      {
        interpolHue = m_maxHue-(elevation*(m_maxHue-m_minHue)/(m_maxElevation-m_minElevation));      //System.out.println("brightness :"+brightness);
        interpolSat = m_maxSat-(elevation*(m_maxSat-m_minSat)/(m_maxElevation-m_minElevation));      
        interpolBri = m_maxBri-(elevation*(m_maxBri-m_minBri)/(m_maxElevation-m_minElevation));  
        
         
    
      }
            
      final Color hsbColor = Color.getHSBColor((float) interpolHue, (float)interpolSat, (float)interpolBri );//
      final Color rgbColor = 
          new Color(
                hsbColor.getRed(), 
                hsbColor.getGreen(), 
                hsbColor.getBlue(),
                m_transparency);
      return rgbColor;  
    }
    else
    {
      throw new IllegalArgumentException(
          "Elevation is out of range:"+
          "\n\tminElevation="+m_minElevation+
          "\n\tmaxElevation="+m_maxElevation+
          "\n\tcurrentElevation="+elevation);
    }
  }
  
  public void setElevationMinMax(
                      double minElevation, 
                      double maxElevation)
  {
    m_minElevation=minElevation;
    m_maxElevation=maxElevation;
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
    else if(elevation>=m_minElevation && elevation<=m_maxElevation)
    {
      double hue = m_minHue+elevation*(m_maxHue-m_minHue)/(m_maxElevation-m_minElevation);
      double sat = m_minSat+elevation*(m_maxSat-m_minSat)/(m_maxElevation-m_minElevation);
      double bri = m_minBri+elevation*(m_maxBri-m_minBri)/(m_maxElevation-m_minElevation);
      return new float[]{ (float)hue, (float)sat, (float)bri};
    }
    else
    {
      //or return a translucent color
      throw new IllegalArgumentException(
          "Elevation is out of range:"+
          "\n\tminElevation="+m_minElevation+
          "\n\tmaxElevation="+m_maxElevation+
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
