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

import java.io.Serializable;

import org.kalypsodeegree.model.geometry.ByteUtils;

/**
 * Class representing a z-range of shape-z.
 * <!---------------------------------------------------------------------------->
 * 
 * @version 19.01.2007
 * @author Thomas Jung
 */

public class SHPZRange implements Serializable
{
  // lowest z-value
  private double m_minZ;

// highest z-value
  private double m_maxZ;

  // ------------- CONSTRUTOR IMPLEMENTATION BEGIN

  public SHPZRange( double minz, double maxz )
  {
    m_minZ = minz; // max. elevation
    m_maxZ = maxz; // min. elevation
  }

  public byte[] writeLESHPZRange( )
  {
    byte[] recBuf = new byte[8 * 4];
    // min z value
    ByteUtils.writeLEDouble( recBuf, 0, m_minZ );
    // max z value
    ByteUtils.writeLEDouble( recBuf, 8, m_maxZ );

    return recBuf;
  }

  public byte[] writeBESHPRange( )
  {
    byte[] recBuf = new byte[8 * 4];
    // west bounding coordinate = xmin of rec-Box
    ByteUtils.writeBEDouble( recBuf, 0, m_minZ );
    // south bounding coordinate = ymin of rec-Box
    ByteUtils.writeBEDouble( recBuf, 8, m_maxZ );

    return recBuf;
  }

  // ----------------- METHOD IMPLEMENTATION
  @Override
  public String toString( )
  {
    return "ZRANGE" + "\n[zmin: " + this.m_minZ + "]" + "\n[zmax: " + this.m_maxZ + "]" + "]";
  }

  public double getMinZ( )
  {
    return m_minZ;
  }

  public void setMinZ( double minZ )
  {
    m_minZ = minZ;
  }

  public double getMaxZ( )
  {
    return m_maxZ;
  }

  public void setMaxZ( double maxZ )
  {
    m_maxZ = maxZ;
  }

}