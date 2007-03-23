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

import java.io.Serializable;

import org.kalypsodeegree.model.geometry.ByteUtils;

/**
 * Class representing a z-range of shape-z.
 * 
  * 
 * <!---------------------------------------------------------------------------->
 * 
 * @version 19.01.2007
 * @author Thomas Jung
 *  
 */

public class SHPZRange implements Serializable
{

  /**
   * this order:
   * 
   * min, max
   */

  // each double 8 byte distance, offset due to position in .shp-file-record
  public static int recZmin = 4;

  public static int recZmax = 12;



  //lowest z-value
  public double zmin;

//highrst z-value
  public double zmax;


  //------------- CONSTRUTOR IMPLEMENTATION BEGIN
  public SHPZRange()
  {

    zmin = 0.0;
    zmax = 0.0;
  

  }
 

  public SHPZRange( double minz, double maxz )
  {

    this.zmin = minz; // max. elevation
    this.zmax = maxz; // min. elevation
    
  }

  /**
   * Transform from WKBPoint to Rectangle
   */
  public SHPZRange( SHPPointz minz, SHPPointz maxz )
  {

    //west bounding coordinate = minEsri.x
    this.zmin = minz.z;
    //east bounding coordinate = maxEsri.x
    this.zmax = maxz.z;


  }

  /**
   * create from an existing SHPZRange
   */
  public SHPZRange( SHPZRange zrang )
  {

    //min
    this.zmin = zrang.zmin;
    //max
    this.zmax = zrang.zmax;
 
  }

  public SHPZRange( byte[] recBuf )
  {

    //west bounding coordinate = xmin of rec-Box
    this.zmin = ByteUtils.readLEDouble( recBuf, recZmin );
    //east bounding coordinate = xmax of rec-Box
    this.zmax = ByteUtils.readLEDouble( recBuf, recZmax );
 

  }

  public byte[] writeLESHPZRange()
  {
    byte[] recBuf = new byte[8 * 4];
    //min z value
    ByteUtils.writeLEDouble( recBuf, 0, zmin );
    //max z value
    ByteUtils.writeLEDouble( recBuf, 8, zmax );


    return recBuf;
  }

  public byte[] writeBESHPRange()
  {
    byte[] recBuf = new byte[8 * 4];
    //west bounding coordinate = xmin of rec-Box
    ByteUtils.writeBEDouble( recBuf, 0, zmin );
    //south bounding coordinate = ymin of rec-Box
    ByteUtils.writeBEDouble( recBuf, 8, zmax );


    return recBuf;
  }

  //----------------- METHOD IMPLEMENTATION
  public String toString()
  {

    return "ZRANGE" + "\n[zmin: " + this.zmin + "]" + "\n[zmax: " + this.zmax + "]" +  "]";

  }

}