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
 * Class representig a collection of pointsz <BR>
 * 
 * <B>Last changes <B>: <BR>
 * 
 * <!---------------------------------------------------------------------------->
 * 
 * @version 16.01.2007
 * @author Thomas Jung
 *  
 */

public class SHPMultiPointz extends SHPGeometry
{
  
  public SHPPointz[] pointsz = null;  
  
  public SHPZRange zrange;
  
  public int numPoints = 0;

  /**
   * constructor: recieves a stream <BR>
   */
  public SHPMultiPointz( byte[] recBuf )
  {

    super( recBuf );
    
    int byteposition = 0;
    
    //bounding box
    envelope = ShapeUtils.readBox( recBuf, 4 );
    
    //number of points
    numPoints = ByteUtils.readLEInt( recBuffer, 36 );

    pointsz = new SHPPointz[numPoints];

    for( int i = 0; i < numPoints; i++ )
    { 
      
      //at first the x- and y-values of the points will be loaded
      byteposition = 40 + i * 16;
      pointsz[i] = new SHPPointz( recBuffer, byteposition );
      
    }
    
    //next the z-range of the pointsz...
    byteposition = 40 + numPoints * 16;
    zrange = ShapeUtils.readZRange( recBuffer,  byteposition );
    

    for( int j = 0; j < numPoints; j++ )
    {
      //at last the z-values of the pointsz...
      byteposition = (40 + numPoints * 16) + 16 + (8 * j);
      pointsz[j].z = ByteUtils.readLEDouble( recBuffer,  byteposition );
    }
  
  }

 
  /**
   * constructor: recieves an array of gm_points
   */

  
  public SHPMultiPointz( GM_MultiPoint multipointz )
  {

    double xmin = multipointz.getEnvelope().getMin().getX();
    double xmax = multipointz.getEnvelope().getMax().getX();
    double ymin = multipointz.getEnvelope().getMin().getY();
    double ymax = multipointz.getEnvelope().getMax().getY();
    double zmin = multipointz.getEnvelope().getMin().getZ();
    double zmax = multipointz.getEnvelope().getMax().getZ();
    
    try
    {
      pointsz = new SHPPointz[multipointz.getSize()];
      for( int i = 0; i < multipointz.getSize(); i++ )
      {
        pointsz[i] = new SHPPointz( multipointz.getPointAt( i ).getPosition() );
        if( pointsz[i].x > xmax )
        {
          xmax = pointsz[i].x;
        }
        else if( pointsz[i].x < xmin )
        {
          xmin = pointsz[i].x;
        }
        if( pointsz[i].y > ymax )
        {
          ymax = pointsz[i].y;
        }
        else if( pointsz[i].y < ymin )
        {
          ymin = pointsz[i].y;
        }
        if( pointsz[i].z > zmax )
        {
          zmax = pointsz[i].z;
        }
        else if( pointsz[i].z < zmin )
        {
          zmin = pointsz[i].z;
        }
      }
    }
    catch( Exception e )
    {
      System.out.println( "SHPMultiPointz::" + e );
    }

  }

  /**
   * method: writeSHPmultipointz (byte [] bytearray, int start) <BR>
   * loops through the pointz array and writes each pointz to the bytearray <BR>
   */
  public byte[] writeSHPMultiPointz( byte[] bytearray, int start )
  {

    int offset = start;

    double xmin = pointsz[0].x;
    double xmax = pointsz[0].x;
    double ymin = pointsz[0].y;
    double ymax = pointsz[0].y;
    double zmin = pointsz[0].z;
    double zmax = pointsz[0].z;
    
    // write shape type identifier ( 18 = multipointz )
    ByteUtils.writeLEInt( bytearray, offset, 18 );

    offset += 4;
    // save offset of the bounding box (filled later)
    int tmp = offset;

    // increment offset with size of the bounding box
    offset += ( 4 * 8 );

    // write number of points
    ByteUtils.writeLEInt( bytearray, offset, pointsz.length );

    offset += 4;

    for( int i = 0; i < pointsz.length; i++ )
    {

      // calculate bounding box
      if( pointsz[i].x > xmax )
      {
        xmax = pointsz[i].x;
      }
      else if( pointsz[i].x < xmin )
      {
        xmin = pointsz[i].x;
      }

      if( pointsz[i].y > ymax )
      {
        ymax = pointsz[i].y;
      }
      else if( pointsz[i].y < ymin )
      {
        ymin = pointsz[i].y;
      }
      
      //calculate z-range
      if( pointsz[i].z > zmax )
      {
        zmax = pointsz[i].z;
      }
      else if( pointsz[i].z < zmin )
      {
        zmin = pointsz[i].z;
      }
      
      // write x-coordinate
      ByteUtils.writeLEDouble( bytearray, offset, pointsz[i].x );

      offset += 8;

      // write y-coordinate
      ByteUtils.writeLEDouble( bytearray, offset, pointsz[i].y );

      offset += 8;

    }

    // jump back to the offset of the bounding box
    offset = tmp;

    // write bounding box to the byte array
    ByteUtils.writeLEDouble( bytearray, offset, xmin );
    offset += 8;
    ByteUtils.writeLEDouble( bytearray, offset, ymin );
    offset += 8;
    ByteUtils.writeLEDouble( bytearray, offset, xmax );
    offset += 8;
    ByteUtils.writeLEDouble( bytearray, offset, ymax );
    
    //jump forward to zmin, zmax
    offset = start + 40 +(16 * pointsz.length);
    
    // write z-range to the byte array
    ByteUtils.writeLEDouble( bytearray, offset, zmin );
    offset += 8;
    ByteUtils.writeLEDouble( bytearray, offset, zmax );
    
    for( int i = 0; i < pointsz.length; i++ )
    {
      offset += 8;
     
      // write z-coordinate
      ByteUtils.writeLEDouble( bytearray, offset, pointsz[i].z );
    }

    return bytearray;
  }

  /**
   * returns the size of the multipointz shape in bytes <BR>
   */
  public int size()
  {
    return 40 + pointsz.length * 16 + 16 + ( 8 * numPoints);
  }

}