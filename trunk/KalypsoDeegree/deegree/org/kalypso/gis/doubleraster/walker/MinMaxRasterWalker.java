/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.gis.doubleraster.walker;

import org.kalypso.gis.doubleraster.DoubleRaster;
import org.kalypso.gis.doubleraster.DoubleRasterWalker;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Determines min/max value of the walked raster.
 * 
 * @author Gernot Belger
 */
public class MinMaxRasterWalker implements DoubleRasterWalker
{
  private double m_min = Double.POSITIVE_INFINITY;

  private double m_max = Double.NEGATIVE_INFINITY;

  /**
   * @see org.kalypso.gis.doubleraster.DoubleRasterWalker#afterLine(int)
   */
  public void afterLine( int y )
  {
  }

  /**
   * Returns null
   * 
   * @see org.kalypso.gis.doubleraster.DoubleRasterWalker#getResult()
   */
  public Object getResult( )
  {
    return null;
  }

  /**
   * @see org.kalypso.gis.doubleraster.DoubleRasterWalker#operate(int, int, com.vividsolutions.jts.geom.Coordinate)
   */
  public void operate( final int x, final int y, final Coordinate c )
  {
    if( !Double.isNaN( c.z ) )
    {
      m_min = Math.min( c.z, m_min );
      m_max = Math.max( c.z, m_max );
    }
  }

  /**
   * @see org.kalypso.gis.doubleraster.DoubleRasterWalker#start(org.kalypso.gis.doubleraster.DoubleRaster)
   */
  public void start( final DoubleRaster r )
  {
  }

  public double getMin( )
  {
    return m_min;
  }

  public double getMax( )
  {
    return m_max;
  }
}
