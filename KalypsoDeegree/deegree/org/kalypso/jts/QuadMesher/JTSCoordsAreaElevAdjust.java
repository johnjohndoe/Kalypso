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
 * In addition there will be an adjustment of interpolated elevations, so that a continued change-over from area 1 (j=0)
 * to area 2 (j=m) for each j-line will be achieved. The elevations for the coordinates (j,0), (j,n),(0,i) and (m,i)
 * will not be changed (outer boundary circle) <code>
 * 
 *  Input:   a completly filled coordinate array fixed z-values in the first and the last row 
 *           and interpolated values everywhere else.
 *  Output:  Coordinate array with adjusted elevations for all coordinates depending on the area characteristics.
 *  
 * 
 *   X----X----X----X----X     - numLong     area 2
 *   |    |    |    |    |     |
 *   X    o    o    o    X     |  
 *   |    |    |    |    |     |
 *   .    .    .    .    .     |
 *                             i
 *   .    .    .    .    .     |
 *   |    |    |    |    |     | 
 *   X    o    o    o    X     | 
 *   |    |    |    |    |     | 
 *   X----X----X----X----X     - 0           area 1
 *   
 *   0                   numCross
 *   |------- j ---------|
 *     
 *   X = fixed elevations
 *   o = elevations will be adjusted
 *  </code>
 * 
 * @author Thomas Jung
 */
public class JTSCoordsAreaElevAdjust
{

  private final Coordinate[][] m_coords;

  public JTSCoordsAreaElevAdjust( final Coordinate[][] coords )
  {
    m_coords = coords;
  }

  public boolean validate( )
  {
    return false;
  }

  /**
   * The elevations will be calculated separately for each i-coordinate array. The z-values will be dirived by the first
   * and last z-coordinate of this array. The interpolation is weighted by the distance of the current point to these
   * two points.
   */
  public Coordinate[][] adjustElevations( )
  {
    final int numCross = m_coords[0].length;
    final int numLong = m_coords.length;

    // calculate areas for j = 0 (start) and j = numCross (end)
    Coordinate lineCoordsStart[] = copyArray(0);
    final double startArea = calculateAreaZ( lineCoordsStart );
    Coordinate lineCoordsEnd[] = copyArray( numCross - 1 );
    final double endArea = calculateAreaZ( lineCoordsEnd );

    for( int j = 1; j < numCross - 2; j++ )  //the values of the first and last row of the coordinate array won't be touched.
    {
      //calculate area ratio
      final double ratio = j / (numCross - 1);
      //calculate the desired target area
      final double targetArea = startArea * (1 - ratio) + endArea * ratio; 
      //calculate the actual area 
      Coordinate lineCoords[] = copyArray(j);

      final double currentArea = calculateAreaZ( lineCoords );
      //calculate the area difference
      final double dArea = targetArea - currentArea;
      //calculate the segment width betwenn the coordinates
      final double[] width = calcWidths( lineCoords );
      //calculate the adjust level
      final double dZ = calcDZ ( width, dArea );
      //adjust the z-values for the inner coordinates
      for ( int i = 1; i < numLong - 2; i++ )
      {
        m_coords[j][i].z = m_coords[j][i].z - dZ;
      }

      //TODO: remove later *************
      /* Just for debug purposes */
      //calculate the actual area again and check if the taken approach is right
      Coordinate lineCoords2[] = copyArray(j);
      final double currentAreaAdjusted = calculateAreaZ( lineCoords2 );
      final double dAreaAdjusted = targetArea - currentAreaAdjusted;
      if( dAreaAdjusted - dArea > 0.10 )
      {
        String s = String.format( "Schlauchgenerator: Fl‰chenausgleich nicht hinreichend genau: %f - %f", targetArea, currentAreaAdjusted );
        System.out.println( s );
      }
      ///********************************
      
    }
    return m_coords;
  }

  /**
   * copies the second dimension of a two-dimensional array into an one-dimensional array
   */
  private Coordinate[] copyArray( int j )
  {
    Coordinate[] coord = new Coordinate[m_coords.length];
    
    for( int i = 0; i < coord.length ; i++ )
    {
      coord[i] = m_coords[i][j];
    }
    return coord;
  }

  private double calcDZ( double[] width, double dArea )
  {
    double dZ = 0;
    double wi = 0;
    //calculate the virt. width of the the first and last segment
    wi = 0.5 * ( width[0] + width [width.length - 1] );
    //add the width of the segments inbetween
    for (int i = 1; i < width.length - 1; i++)
    {
      wi = wi + width[i];
    }
    dZ = dArea / wi;
    return dZ;
  }

  private double[] calcWidths( Coordinate[] lineCoords )
  {
    final double[] width = new double [lineCoords.length - 2];
    
    for (int i = 0; i < lineCoords.length - 2; i++)
    {
      width[i] = lineCoords[i].distance( lineCoords[i+1] );
    }
    return width;
  }

  /**
   * calculates the area z value of a coordinate array
   */
  private double calculateAreaZ( Coordinate[] line_coords)
  {
    double width = 0;
    double area = 0;
    double maxZ = 0;
    
    maxZ = calcMaxZ (line_coords);
    //lower coordinates by minZ to avoid negativ elevations during the area calculation
    for( int i = 0; i < line_coords.length - 1; i++ )
    {   
      final double z1 = (maxZ - line_coords[i].z);
      final double z2 = (maxZ - line_coords[i+1].z);
      width = line_coords[i].distance( line_coords[i+1] );
      area =  area + ( z1 + z2 ) / 2 * width;
    }
    return area;
  }

  /**
   * gives the maximal z value of a coordinate array
   */
    private double calcMaxZ( Coordinate[] line_coords )
    {
      double maxZ = -9999;
      
      for ( int i = 0; i < line_coords.length ; i++ )
      {
        if ( maxZ < line_coords[i].z )
          maxZ = line_coords[i].z;
      }
      return maxZ;
    }
}
