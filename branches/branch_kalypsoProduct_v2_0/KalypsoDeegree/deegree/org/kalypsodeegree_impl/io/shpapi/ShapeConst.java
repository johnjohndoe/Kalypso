/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */

package org.kalypsodeegree_impl.io.shpapi;

/**
 * Class containing all constants needed for reading of a shape file <BR>
 * <B>Last changes <B>: <BR>
 * 21.12.1999 ap: all constants declared <BR>
 * <!---------------------------------------------------------------------------->
 * 
 * @version 14.12.1999
 * @author Andreas Poth
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
  public static final byte SHAPE_TYPE_NULL = 0;

  /**
   * The indicator for a point shape type. (1)
   */
  public static final byte SHAPE_TYPE_POINT = 1;

  /**
   * The indicator for an polyline shape type. (3)
   */
  public static final byte SHAPE_TYPE_POLYLINE = 3;

  /**
   * The indicator for a polygon shape type. (5)
   */
  public static final byte SHAPE_TYPE_POLYGON = 5;

  /**
   * The indicator for a multipoint shape type. (8)
   */
  public static final byte SHAPE_TYPE_MULTIPOINT = 8;

  /**
   * start point of field parts in ESRI shape record
   */
  public static final int PARTS_START = 44;

  /**
   * The indicator for a point shape type. (11)
   */
  public static final byte SHAPE_TYPE_POINTZ = 11;

  /**
   * The indicator for an polyline shape type. (13)
   */
  public static final byte SHAPE_TYPE_POLYLINEZ = 13;

  /**
   * The indicator for a polygon shape type. (15)
   */
  public static final byte SHAPE_TYPE_POLYGONZ = 15;

  /**
   * The indicator for a multipoint shape type. (18)
   */
  public static final byte SHAPE_TYPE_MULTIPOINTZ = 18;

  public static String getShapeConstantAsString( final byte shpType )
  {
    final String string = null;

    switch( shpType )
    {
      case 0:
        return "Null";

      case 1:
        return "Point";

      case 3:
        return "PolyLine";

      case 5:
        return "Polygon";

      case 8:
        return "MultiPoint";

      case 11:
        return "PointZ";

      case 13:
        return "PolyLineZ";

      case 15:
        return "PolygonZ";
      case 18:
        return "MultiPointZ";
      case 21:
        return "PointM";
      case 23:
        return "PolyLineM";
      case 25:
        return "PolygonM";
      case 28:
        return "MultiPointM";
      case 31:
        return "MultiPatch";
    }
    return string;

  }

}
