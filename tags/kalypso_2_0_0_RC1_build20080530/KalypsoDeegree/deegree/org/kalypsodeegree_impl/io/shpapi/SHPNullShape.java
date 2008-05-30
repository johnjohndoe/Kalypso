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

import org.kalypsodeegree.model.geometry.ByteUtils;

/**
 * Class representig an empty geometry <BR>
 * <B>Last changes <B>: <BR>
 * update 04.10.2007, Jung, implemented writeNullShape method
 * <!---------------------------------------------------------------------------->
 * 
 * @version 25.1.2000
 * @author Andreas Poth <br>
 *         Thomas Jung
 */

public class SHPNullShape implements ISHPGeometry
{

  /**
   * method: writeNullShape: writes a NullShape Object to a recBuffer <BR>
   */
  public byte[] writeShape( )
  {
    int offset = ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH;
    final byte[] byteArray = new byte[offset + size()];

    // write shape type identifier ( 0 = null shape )
    ByteUtils.writeLEInt( byteArray, offset, 0 );

    offset += 4;

    return byteArray;
  }

  /**
   * returns the polygon shape size in bytes <BR>
   */
  public int size( )
  {
    return 12;
  }

  /**
   * @see org.kalypsodeegree_impl.io.shpapi.SHPGeometry#getEnvelope()
   */
  public SHPEnvelope getEnvelope( )
  {
    return null;
  }

}