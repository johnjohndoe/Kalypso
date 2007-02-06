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
package org.kalypso.jts.QuadMesher;

import com.vividsolutions.jts.geom.Coordinate;


/**
 * Interpolates elevations for an m x n coordinate array derived by elevations placed on the first and last row of the
 * coordinate array (see below). 
 *
 * <code>
 * 
 *  Input:   completly filled coordinate array with z-values in the first and the last row.
 *  Output:  Coordinate array with interpolated elevations for all coordinates.
 *  
 * 
 *   X----X----X----X----X     - numLong     
 *   |    |    |    |    |     |
 *   o    o    o    o    o     |  
 *   |    |    |    |    |     |
 *   .    .    .    .    .     |
 *                             i
 *   .    .    .    .    .     |
 *   |    |    |    |    |     | 
 *   o    o    o    o    o     | 
 *   |    |    |    |    |     | 
 *   X----X----X----X----X     - 0          
 *   
 *   0                   numCross
 *   |------- j ---------|
 *     
 *   X = existing elevations
 *   o = elevations will be interpolated
 *  
 *  
 *  </code>
 * 
 * @author Thomas Jung
 */
public class JTSCoordsElevInterpol
{

  private final Coordinate[][] m_coords;

  public JTSCoordsElevInterpol( final Coordinate[][] coords )
  {
    m_coords = coords;
  }

  public boolean validate( )
  {
    return false;
  }

  /**
   * The elevations will be calculated separately for each i-coordinate array. 
   * The z-values will be dirived by the first and last z-coordinate of this array. 
   * The interpolation is weighted by the distance of the current point to these
   * two points.
   */
  public Coordinate[][] calculateElevations( )
  {
    final int numCross = m_coords[0].length;
    final int numLong = m_coords.length;

    for( int j = 0; j < numCross; j++ )
    {
      final double zStart = m_coords[j][0].z;
      final double zEnd = m_coords[j][numCross-1].z;
      double distTotal = 0;
      double dist = 0;
      double ratio;

      // calculate the route from along the coordinates
      distTotal = calcRoute( j, 0, numLong );

      for( int i = 1; i < numLong - 1; i++ )
      {
        // current route calculation between first point and current point
        dist = dist + m_coords[j][i].distance( m_coords[j][i-1] );

        // the z-values will be weighted by the ratio between the current route
        // and the whole route (= distance to the z-value-coordinates)
        ratio = dist / distTotal;

        m_coords[j][i].z = zStart * (1 - ratio) + zEnd * ratio;
      }
    }
    return m_coords;
  }

  /**
   * Calculates the route through a coordinate array j from start point to end point
   */
  private double calcRoute( int j, int startPoint, int endPoint )
  {
    double route = 0;
    for( int i = startPoint + 1; i < endPoint; i++ )
    {
      // route calculation for the whole way along the coordinates
      route = route + m_coords[j][i].distance( m_coords[j][i-1] );
    }
    return route;
  }

}
