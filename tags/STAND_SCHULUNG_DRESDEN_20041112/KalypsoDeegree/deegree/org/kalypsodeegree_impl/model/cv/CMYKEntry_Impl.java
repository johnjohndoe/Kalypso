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

import org.deegree.model.coverage.CMYKEntry;

/**
 * describes an entry to a color table in CMYK format
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
public class CMYKEntry_Impl extends PseudoColorTableEntry_Impl implements CMYKEntry
{

  private float black = 0;

  private float cyan = 0;

  private float magenta = 0;

  private float yellow = 0;

  public CMYKEntry_Impl( int index, float cyan, float magenta, float yellow, float black )
  {
    super( index );
    this.cyan = cyan;
    this.magenta = magenta;
    this.yellow = yellow;
    this.black = black;
  }

  /**
   * returns the black component (0..1)
   *  
   */
  public float getBlack()
  {
    return black;
  }

  /**
   * returns the cyan component (0..1)
   *  
   */
  public float getCyan()
  {
    return cyan;
  }

  /**
   * returns the magenta component (0..1)
   *  
   */
  public float getMagenta()
  {
    return magenta;
  }

  /**
   * returns the yellow component (0..1)
   *  
   */
  public float getYellow()
  {
    return yellow;
  }

  public String toString()
  {
    String ret = "CMYKEntry_Impl: \n";
    ret += "cyan: " + cyan + "\n";
    ret += "magenta: " + magenta + "\n";
    ret += "yellow: " + yellow + "\n";
    ret += "black: " + black;
    return ret;
  }

}