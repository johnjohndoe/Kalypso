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

/**
 * Utilities for reading and writing the components of shape files.
 * 
 * 
 * <B>Last changes <B>: <BR>
 * 25.11.1999 ap: memory allocation dynaminized <BR>
 * 17.1.2000 ap: method SHPPoint readPoint(byte[] b, int off) modified <BR>
 * 17.1.2000 ap: method SHPEnvelope readBox(byte[] b, int off) modified <BR>
 * 17.1.2000 ap: method writePoint(..) modified <BR>
 * 
 * <!---------------------------------------------------------------------------->
 * 
 * @version 25.1.2000
 * @author Andreas Poth
 *  
 */

public class ShapeUtils
{

  /**
   * readPoint(byte[] b, int off) <BR>
   * Reads a point record. A point record is a double representing the x value
   * and a double representing a y value.
   * 
   * @param b
   *          the raw data buffer
   * @param off
   *          the offset into the buffer where the int resides
   * @return the point read from the buffer at the offset location
   */
  public static SHPPoint readPoint( byte[] b, int off )
  {

    SHPPoint point = new SHPPoint();

    point.x = ByteUtils.readLEDouble( b, off );
    point.y = ByteUtils.readLEDouble( b, off + 8 );

    return point;

  }

  /**
   * method: readBox(byte[] b, int off) <BR>
   * Reads a bounding box record. A bounding box is four double representing, in
   * order, xmin, ymin, xmax, ymax.
   * 
   * @param b
   *          the raw data buffer
   * @param off
   *          the offset into the buffer where the int resides
   * @return the point read from the buffer at the offset location
   */
  public static SHPEnvelope readBox( byte[] b, int off )
  {

    SHPEnvelope bb = new SHPEnvelope();

    SHPPoint min = readPoint( b, off );
    SHPPoint max = readPoint( b, off + 16 );

    bb.west = min.x;
    bb.south = min.y;
    bb.east = max.x;
    bb.north = max.y;

    return bb;

  }

  /**
   * method: writePoint(byte[] b, int off, ESRIPoint point) <BR>
   * Writes the given point to the given buffer at the given location. The point
   * is written as a double representing x followed by a double representing y.
   * 
   * @param b
   *          the data buffer
   * @param off
   *          the offset into the buffer where writing should occur
   * @param point
   *          the point to write
   * @return the number of bytes written
   */
  public static int writePoint( byte[] b, int off, SHPPoint point )
  {

    int nBytes = ByteUtils.writeLEDouble( b, off, point.x );

    nBytes += ByteUtils.writeLEDouble( b, off + nBytes, point.y );

    return nBytes;

  }

  /**
   * method: writeBox(byte[] b, int off, ESRIBoundingBox box) <BR>
   * Writes the given bounding box to the given buffer at the given location.
   * The bounding box is written as four doubles representing, in order, xmin,
   * ymin, xmax, ymax.
   * 
   * @param b
   *          the data buffer
   * @param off
   *          the offset into the buffer where writing should occur
   * @param box
   *          the bounding box to write
   * @return the number of bytes written
   */
  public static int writeBox( byte[] b, int off, SHPEnvelope box )
  {

    SHPPoint min = new SHPPoint();
    min.x = box.west;
    min.y = box.south;
    SHPPoint max = new SHPPoint();
    max.x = box.east;
    max.y = box.north;

    int nBytes = writePoint( b, off, min );

    nBytes += writePoint( b, off + nBytes, max );

    return nBytes;

  }

}