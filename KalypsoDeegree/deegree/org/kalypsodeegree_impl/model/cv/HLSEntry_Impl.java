/*----------------    FILE HEADER  ------------------------------------------
 
 This file is part of deegree (Java Framework for Geospatial Solutions).
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de
 
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
 
 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de
 
 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de
 
 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.model.cv;

import org.deegree.model.coverage.HLSEntry;

/**
 * HLS entry of a color table
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
public class HLSEntry_Impl extends PseudoColorTableEntry_Impl implements HLSEntry
{

  private float hue = 0;

  private float lightness = 0;

  private float saturation = 0;

  public HLSEntry_Impl( int index, float hue, float lightness, float saturation )
  {
    super( index );
    this.hue = hue;
    this.lightness = lightness;
    this.saturation = saturation;
  }

  /**
   * returns the hue value of a HLS entry (0..1)
   *  
   */
  public float getHue()
  {
    return hue;
  }

  /**
   * returns the lightness value of a HLS entry (0..1)
   *  
   */
  public float getLightness()
  {
    return lightness;
  }

  /**
   * returns the saturation value of a HLS entry (0..1)
   *  
   */
  public float getSaturation()
  {
    return saturation;
  }

}