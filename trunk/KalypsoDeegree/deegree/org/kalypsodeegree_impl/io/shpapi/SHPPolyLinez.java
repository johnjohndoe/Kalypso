/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

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


 history:

 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always.

 If you intend to use this software in other ways than in kalypso
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree,
 original copyright:

 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de

 ---------------------------------------------------------------------------------------------------*/

package org.kalypsodeegree_impl.io.shpapi;

import org.kalypsodeegree.model.geometry.ByteUtils;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_LineString;

/**
 * Class representig a two dimensional ESRI PolyLine <BR>
 * <B>Last changes <B>: <BR>
 * <!---------------------------------------------------------------------------->
 * 
 * @version 19.01.2007
 * @author Thomas Jung
 */

public class SHPPolyLinez implements ISHPGeometry
{

  public int numParts;

  public int numPoints;

  public SHPPointz[][] pointsz = null;

  public final SHPZRange m_zrange;

  private final SHPEnvelope m_envelope;

  /**
   * constructor: gets a stream <BR>
   */
  public SHPPolyLinez( byte[] recBuf )
  {

    int pointsStart = 0;
    int sumPoints = 0;
    int byteposition = 0; // position of the bytes to be read

    // bounding box
    byteposition = 4;
    m_envelope = ShapeUtils.readBox( recBuf, byteposition );

    // number of line parts
    byteposition = 36;
    numParts = ByteUtils.readLEInt( recBuf, byteposition );

    // total number of points
    byteposition = 40;
    numPoints = ByteUtils.readLEInt( recBuf, byteposition );

    // index of the first point in part
    pointsStart = ShapeConst.PARTS_START + (numParts * 4);

    // array of points for all parts
    pointsz = new SHPPointz[numParts][];

    // get the index for the first point of each part
    for( int j = 0; j < numParts; j++ )
    {

      int firstPointNo = 0;
      int nextFirstPointNo = 0;
      int offset = 0;
      int lnumPoints = 0;

      // get number of first point of current part out of ESRI shape Record:
      firstPointNo = ByteUtils.readLEInt( recBuf, ShapeConst.PARTS_START + (j * 4) );

      // calculate offset of part in bytes, count from the beginning of
      // recordbuffer
      offset = pointsStart + (firstPointNo * 16);

      // get number of first point of next part ...
      if( j < numParts - 1 )
      {
        // ... usually from ESRI shape Record
        nextFirstPointNo = ByteUtils.readLEInt( recBuf, ShapeConst.PARTS_START + ((j + 1) * 4) );
      }
      // ... for the last part as total number of points
      else if( j == numParts - 1 )
      {
        nextFirstPointNo = numPoints;
      }

      // calculate number of points per part due to distance and
      // calculate some checksum for the total number of points to be worked
      lnumPoints = nextFirstPointNo - firstPointNo;
      sumPoints += lnumPoints;

      // allocate memory for the j-th part
      pointsz[j] = new SHPPointz[lnumPoints];

      // create the points of the j-th part from the buffer
      for( int i = 0; i < lnumPoints; i++ )
      {
        // allocate memory for the points of the j-th part
        pointsz[j][i] = new SHPPointz( recBuf, offset + (i * 16) );

        // jump to the z-values of the points
        byteposition = 44 + (4 * numParts) + (i * 16);

        pointsz[j][i].setZ( ByteUtils.readLEDouble( recBuf, byteposition ) );
      }
      byteposition = offset;
    }

    // next the z-range of the pointsz...
    byteposition = 44 + (4 * numParts) + (numPoints * 16);
    m_zrange = ShapeUtils.readZRange( recBuf, byteposition );

  }

  /**
   * constructor: recieves a matrix of GM_Points <BR>
   */
  public SHPPolyLinez( GM_Curve[] curve )
  {

    double xmin = curve[0].getEnvelope().getMin().getX();
    double xmax = curve[0].getEnvelope().getMax().getX();
    double ymin = curve[0].getEnvelope().getMin().getY();
    double ymax = curve[0].getEnvelope().getMax().getY();
    double zmin = curve[0].getEnvelope().getMin().getZ();
    double zmax = curve[0].getEnvelope().getMax().getZ();

    numParts = curve.length;

    numPoints = 0;

    pointsz = new SHPPointz[numParts][];

    try
    {
      // create SHPPoints from the GM_Points array
      for( int i = 0; i < numParts; i++ )
      {

        GM_LineString ls = curve[i].getAsLineString();

        numPoints += ls.getNumberOfPoints();

        pointsz[i] = new SHPPointz[ls.getNumberOfPoints()];

        for( int j = 0; j < ls.getNumberOfPoints(); j++ )
        {
          pointsz[i][j] = new SHPPointz( ls.getPositionAt( j ) );
          if( pointsz[i][j].getX() > xmax )
          {
            xmax = pointsz[i][j].getX();
          }
          else if( pointsz[i][j].getX() < xmin )
          {
            xmin = pointsz[i][j].getX();
          }
          if( pointsz[i][j].getY() > ymax )
          {
            ymax = pointsz[i][j].getY();
          }
          else if( pointsz[i][j].getY() < ymin )
          {
            ymin = pointsz[i][j].getY();
          }
          if( pointsz[i][j].getZ() > zmax )
          {
            zmax = pointsz[i][j].getZ();
          }
          else if( pointsz[i][j].getZ() < zmin )
          {
            zmin = pointsz[i][j].getZ();
          }
        }

      }
    }
    catch( Exception e )
    {
      System.out.println( "SHPPolyLine:: " + e );
    }

    m_envelope = new SHPEnvelope( xmin, xmax, ymax, ymin );
    m_zrange = new SHPZRange( zmin, zmax );

  }

  /**
   * method: writeSHPPolyLine(byte[] bytearray, int start) <BR>
   */
  public byte[] writeShape( )
  {
    int offset = ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH;
    final byte[] byteArray = new byte[offset + size()];

    double xmin = pointsz[0][0].getX();
    double xmax = pointsz[0][0].getX();
    double ymin = pointsz[0][0].getY();
    double ymax = pointsz[0][0].getY();
    double zmin = pointsz[0][0].getZ();
    double zmax = pointsz[0][0].getZ();

    int byteposition;

    // write shape type identifier ( 3 = polyline )
    ByteUtils.writeLEInt( byteArray, offset, 3 );

    offset += 4;
    // save offset of the bounding box
    int tmp1 = offset;

    // increment offset with size of the bounding box
    offset += (4 * 8);

    // write numparts
    ByteUtils.writeLEInt( byteArray, offset, numParts );
    offset += 4;
    // write numpoints
    ByteUtils.writeLEInt( byteArray, offset, numPoints );
    offset += 4;

    // save offset of the list of offsets for each polyline
    int tmp2 = offset;

    // increment offset with numParts
    offset += (4 * numParts);

    int count = 0;
    for( int i = 0; i < pointsz.length; i++ )
    {

      // stores the index of the i'th part
      ByteUtils.writeLEInt( byteArray, tmp2, count );
      tmp2 += 4;

      // write the points of the i'th part and calculate bounding box and z-range
      for( int j = 0; j < pointsz[i].length; j++ )
      {

        count++;

        // calculate bounding box
        if( pointsz[i][j].getX() > xmax )
        {
          xmax = pointsz[i][j].getX();
        }
        else if( pointsz[i][j].getX() < xmin )
        {
          xmin = pointsz[i][j].getX();
        }

        if( pointsz[i][j].getY() > ymax )
        {
          ymax = pointsz[i][j].getY();
        }
        else if( pointsz[i][j].getY() < ymin )
        {
          ymin = pointsz[i][j].getY();
        }

        if( pointsz[i][j].getZ() > zmax )
        {
          zmax = pointsz[i][j].getZ();
        }
        else if( pointsz[i][j].getZ() < zmin )
        {
          zmin = pointsz[i][j].getZ();
        }

        // write x-coordinate
        ByteUtils.writeLEDouble( byteArray, offset, pointsz[i][j].getX() );
        offset += 8;

        // write y-coordinate
        ByteUtils.writeLEDouble( byteArray, offset, pointsz[i][j].getY() );
        offset += 8;

        // write z-coordinate
        // jump to the z-values
        byteposition = 44 + (4 * numParts) + (count * 16);
        ByteUtils.writeLEDouble( byteArray, byteposition, pointsz[i][j].getZ() );

      }

    }

    // jump back to the offset of the bounding box
    offset = tmp1;

    // write bounding box to the byte array
    ByteUtils.writeLEDouble( byteArray, offset, xmin );
    offset += 8;
    ByteUtils.writeLEDouble( byteArray, offset, ymin );
    offset += 8;
    ByteUtils.writeLEDouble( byteArray, offset, xmax );
    offset += 8;
    ByteUtils.writeLEDouble( byteArray, offset, ymax );

    // write z-range
    // jump to the z-range byte postition
    byteposition = 44 + (4 * numParts) + (numPoints * 16);
    // write z-range to the byte array
    ByteUtils.writeLEDouble( byteArray, byteposition, zmin );
    offset += 8;
    ByteUtils.writeLEDouble( byteArray, byteposition, zmax );

    return byteArray;
  }

  /**
   * returns the polyline shape size in bytes <BR>
   */
  public int size( )
  {
    return 44 + numParts * 4 + numPoints * 16 + 16 + (8 * numPoints);
  }

  public SHPEnvelope getEnvelope( )
  {
    return m_envelope;
  }

  public SHPZRange getZRange( )
  {
    return m_zrange;
  }

} // end of class PolyLine
