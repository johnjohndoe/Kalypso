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

/**
 * Class containing all constants needed for reading of a shape file <BR>
 * 
 * <B>Last changes <B>: <BR>
 * 21.12.1999 ap: all constants declared <BR>
 * 
 * <!---------------------------------------------------------------------------->
 * 
 * @version 14.12.1999
 * @author Andreas Poth
 *  
 */

public class ShapeConst
{

  /**
   * The length of a shape file record header in bytes. (8)
   */
  public static final int SHAPE_FILE_RECORD_HEADER_LENGTH = 8;

  /**
   * The length of a shape file header in bytes. (100)
   */
  public static final int SHAPE_FILE_HEADER_LENGTH = 100;

  /**
   * A Shape File's magic number.
   */
  public static final int SHAPE_FILE_CODE = 9994;

  /**
   * The currently handled version of Shape Files.
   */
  public static final int SHAPE_FILE_VERSION = 1000;

  /**
   * The indicator for a null shape type. (0)
   */
  public static final int SHAPE_TYPE_NULL = 0;

  /**
   * The indicator for a point shape type. (1)
   */
  public static final int SHAPE_TYPE_POINT = 1;

  /**
   * The indicator for an polyline shape type. (3)
   */
  public static final int SHAPE_TYPE_POLYLINE = 3;

  /**
   * The indicator for a polygon shape type. (5)
   */
  public static final int SHAPE_TYPE_POLYGON = 5;

  /**
   * The indicator for a multipoint shape type. (8)
   */
  public static final int SHAPE_TYPE_MULTIPOINT = 8;

  /**
   * start point of field parts in ESRI shape record
   */
  public static final int PARTS_START = 44;

}

