/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
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

package org.deegree_impl.io.shpapi;

import org.deegree.model.geometry.ByteUtils;
import org.deegree.model.geometry.GM_Position;

/**
 * Class representig a two dimensional point <BR>
 * 
 * <B>Last changes <B>: <BR>
 * 25.05.00 chm: method writeSHPPoint implemented <BR>
 * 14.08.00 ap: import clause added <BR>
 * 
 * <!---------------------------------------------------------------------------->
 * 
 * @version 14.08.2000
 * @author Andreas Poth
 *  
 */

public class SHPPoint extends SHPGeometry
{

  public double x;

  public double y;

  public SHPPoint()
  {}

  /**
   * constructor: gets a stream and the start index <BR>
   * of point on it <BR>
   */
  public SHPPoint( byte[] recBuf, int xStart )
  {

    super( recBuf );

    //get x out of recordbuffer
    this.x = ByteUtils.readLEDouble( recBuffer, xStart );
    //get y out of recordbuffer
    this.y = ByteUtils.readLEDouble( recBuffer, xStart + 8 );

  }

  /**
   * constructor: creates a SHPPoint from a WKS Geometrie <BR>
   */
  public SHPPoint( GM_Position point )
  {
    x = point.getX();
    y = point.getY();
  }

  /**
   * method: writeSHPPoint: writes a SHPPoint Objekt to a recBuffer <BR>
   */
  public void writeSHPPoint( byte[] byteArray, int start )
  {

    int offset = start;

    // write shape type identifier ( 1 = point )
    ByteUtils.writeLEInt( byteArray, offset, 1 );

    offset += 4;

    //write x into the recbuffer
    ByteUtils.writeLEDouble( byteArray, offset, x );

    offset += 8;

    //write y into the recbuffer
    ByteUtils.writeLEDouble( byteArray, offset, y );

  }

  /**
   * returns the size of the point shape in bytes <BR>
   */
  public int size()
  {
    return 20;
  }

  public String toString()
  {

    return "SHPPOINT" + "[" + this.x + "; " + this.y + "]";

  }

}