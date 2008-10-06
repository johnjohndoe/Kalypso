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
import org.kalypsodeegree.model.geometry.GM_MultiPoint;

/**
 * Class representig a collection of points <BR>
 * <B>Last changes <B>: <BR>
 * 21.03.2000 ap: constructor declared and implemented <BR>
 * 14.08.2000 ap: constructor SHPMultiPoint(GM_Point[] gm_points) added <BR>
 * 14.08.2000 ap: method writeSHPMultiPoint(..) added <BR>
 * 14.08.2000 ap: import clause added <BR>
 * 16.08.2000 ap: constructor SHPMultiPoint(GM_Point[] gm_points) modified <BR>
 * <!---------------------------------------------------------------------------->
 * 
 * @version 16.08.2000
 * @author Andreas Poth
 */

public class SHPMultiPoint implements ISHPGeometry
{
  public SHPPoint[] points = null;

  public int numPoints = 0;

  private final SHPEnvelope m_envelope;

  /**
   * constructor: recieves a stream <BR>
   */
  public SHPMultiPoint( byte[] recBuf )
  {

    m_envelope = ShapeUtils.readBox( recBuf, 4 );

    numPoints = ByteUtils.readLEInt( recBuf, 36 );

    points = new SHPPoint[numPoints];

    for( int i = 0; i < numPoints; i++ )
    {
      points[i] = new SHPPoint( recBuf, 40 + i * 16 );
    }

  }

  /**
   * constructor: recieves an array of gm_points
   */
  public SHPMultiPoint( GM_MultiPoint multipoint )
  {

    double xmin = multipoint.getEnvelope().getMin().getX();
    double xmax = multipoint.getEnvelope().getMax().getX();
    double ymin = multipoint.getEnvelope().getMin().getY();
    double ymax = multipoint.getEnvelope().getMax().getY();

    try
    {
      points = new SHPPoint[multipoint.getSize()];
      for( int i = 0; i < multipoint.getSize(); i++ )
      {
        points[i] = new SHPPoint( multipoint.getPointAt( i ) );
        if( points[i].getX() > xmax )
        {
          xmax = points[i].getX();
        }
        else if( points[i].getX() < xmin )
        {
          xmin = points[i].getX();
        }
        if( points[i].getY() > ymax )
        {
          ymax = points[i].getY();
        }
        else if( points[i].getY() < ymin )
        {
          ymin = points[i].getY();
        }
      }
    }
    catch( Exception e )
    {
      System.out.println( "SHPMultiPoint::" + e );
    }

    m_envelope = new SHPEnvelope( xmin, xmax, ymax, ymin );

  }

  /**
   * method: writeSHPmultipoint (byte [] bytearray, int start) <BR>
   * loops through the point array and writes each point to the bytearray <BR>
   */
  public byte[] writeShape( )
  {

    int offset = ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH;
    final byte[] byteArray = new byte[offset + size()];

    double xmin = points[0].getX();
    double xmax = points[0].getX();
    double ymin = points[0].getY();
    double ymax = points[0].getY();

    // write shape type identifier ( 8 = multipoint )
    ByteUtils.writeLEInt( byteArray, offset, 8 );

    offset += 4;
    // save offset of the bounding box
    int tmp = offset;

    // increment offset with size of the bounding box
    offset += (4 * 8);

    // write number of points
    ByteUtils.writeLEInt( byteArray, offset, points.length );

    offset += 4;

    for( int i = 0; i < points.length; i++ )
    {

      // calculate bounding box
      if( points[i].getX() > xmax )
      {
        xmax = points[i].getX();
      }
      else if( points[i].getX() < xmin )
      {
        xmin = points[i].getX();
      }

      if( points[i].getY() > ymax )
      {
        ymax = points[i].getY();
      }
      else if( points[i].getY() < ymin )
      {
        ymin = points[i].getY();
      }

      // write x-coordinate
      ByteUtils.writeLEDouble( byteArray, offset, points[i].getX() );

      offset += 8;

      // write y-coordinate
      ByteUtils.writeLEDouble( byteArray, offset, points[i].getY() );

      offset += 8;

    }

    // jump back to the offset of the bounding box
    offset = tmp;

    // write bounding box to the byte array
    ByteUtils.writeLEDouble( byteArray, offset, xmin );
    offset += 8;
    ByteUtils.writeLEDouble( byteArray, offset, ymin );
    offset += 8;
    ByteUtils.writeLEDouble( byteArray, offset, xmax );
    offset += 8;
    ByteUtils.writeLEDouble( byteArray, offset, ymax );

    return byteArray;
  }

  /**
   * returns the size of the multipoint shape in bytes <BR>
   */
  public int size( )
  {
    return 40 + points.length * 16;
  }

  /**
   * @see org.kalypsodeegree_impl.io.shpapi.SHPGeometry#getEnvelope()
   */
  public SHPEnvelope getEnvelope( )
  {
    return m_envelope;
  }

}