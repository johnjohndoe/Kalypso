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

import org.deegree.model.coverage.RGBAEnty;

/**
 * the interface defines a RGBA entry to a color table
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
class RGBAEnty_Impl extends PseudoColorTableEntry_Impl implements RGBAEnty
{

  private float red = 0;

  private float green = 0;

  private float blue = 0;

  private float alpha = 0;

  RGBAEnty_Impl( int index, float red, float green, float blue, float alpha )
  {
    super( index );
    this.red = red;
    this.green = green;
    this.blue = blue;
    this.alpha = alpha;
  }

  /**
   * returns the red component of the color table entry (0..1)
   *  
   */
  public float getRed()
  {
    return red;
  }

  /**
   * returns the green component of the color table entry (0..1)
   *  
   */
  public float getGreen()
  {
    return green;
  }

  /**
   * returns the blue component of the color table entry (0..1)
   *  
   */
  public float getBlue()
  {
    return blue;
  }

  /**
   * returns the alpha component of the color table entry (0..1)
   *  
   */
  public float getAlpha()
  {
    return alpha;
  }

}