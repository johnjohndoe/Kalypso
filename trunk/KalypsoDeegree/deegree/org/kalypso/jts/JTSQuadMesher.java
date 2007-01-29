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
package org.kalypso.jts;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;

/**
 * Generates a quad-mesh from four lines with end / start points which are coincident and build a quadrangle.
 * 
 * @author Thomas Jung
 */
public class JTSQuadMesher
{

  private final LineString m_bottomLine;

  private final LineString m_rightLine;

  private final LineString m_leftLine;

  private final LineString m_topLine;

  public JTSQuadMesher( final LineString topLine, final LineString bottomLine, final LineString leftLine, final LineString rightLine )
  {
    m_topLine = topLine;
    m_bottomLine = bottomLine;
    m_leftLine = leftLine;
    m_rightLine = rightLine;
  }

  public boolean validate( )
  {

    return false;
  }

  /**
   * check the orientation of the lines for the algorithm the line nodes must be oriented according the bootom line node
   * orientation. finaly, <code>
   *    - the bottom line won't be changed
   *    - the top line has to be oriented like the bottom line
   *    - the left and the right line have to be oriented from 
   *      bottom line to top line. 
   *      
   *              top line
   *            O--->---->O  
   *            ^         ^
   *       left |         |  right
   *            |         |
   *            O--->---->O 
   *            bottom line
   * </code>
   */
  public Coordinate[][] calculateMesh( )
  {
    final Coordinate[] coordinatesBottom = m_bottomLine.getCoordinates();
    final Coordinate[] coordinatesTop = m_topLine.getCoordinates();
    final Coordinate[] coordinatesLeft = m_leftLine.getCoordinates();
    final Coordinate[] coordinatesRight = m_rightLine.getCoordinates();

    final LineString newTop;
    final LineString newBottom;
    final LineString newLeft;
    final LineString newRight;

    // check orientation of the line strings (see above)

    // the begin of the left sided line string must identical with the begin of the bottom line string, if not then flip
    // the line
    if( coordinatesLeft[coordinatesLeft.length - 1].equals2D( coordinatesBottom[0] ) )
    {
      newLeft = LineStringUtilities.changeOrientation( m_leftLine );
    }
    else
    {
      newLeft = m_leftLine;
    }

    // the begin of the right sided line string must identical with the end of the bottom line string, if not then flip
    // the line
    if( coordinatesRight[coordinatesRight.length - 1].equals2D( coordinatesBottom[coordinatesBottom.length - 1] ) )
    {
      newRight = LineStringUtilities.changeOrientation( m_rightLine );
    }
    else
    {
      newRight = m_rightLine;
    }

    // the begin of the top sided line string must identical with the end of the left line string, if not then flip the
    // line
    if( coordinatesTop[coordinatesTop.length - 1].equals2D( coordinatesLeft[coordinatesLeft.length - 1] ) )
    {
      newTop = LineStringUtilities.changeOrientation( m_topLine );
    }
    else
    {
      newTop = m_topLine;
    }

    // the bootom line is always well oriented!
    newBottom = m_bottomLine;

    final Coordinate[] coordinatesNewBottom = newBottom.getCoordinates();
    final Coordinate[] coordinatesNewTop = newTop.getCoordinates();
    final Coordinate[] coordinatesNewLeft = newLeft.getCoordinates();
    final Coordinate[] coordinatesNewRight = newRight.getCoordinates();

    final Coordinate[][] meshPoints = new Coordinate[coordinatesNewBottom.length][coordinatesNewLeft.length];

    // calculate dictance between the first and the last intersection points for the new_top and new_bottom.
    final double distBottom = coordinatesNewBottom[0].distance( coordinatesNewBottom[coordinatesNewBottom.length - 1] );
    final double distTop = coordinatesNewTop[0].distance( coordinatesNewTop[coordinatesNewTop.length - 1] );

    for( int j = 0; j < coordinatesNewBottom.length; j++ )
    {

      // calculate distance between each bottom line point and the starting point of the bottom line
      final double distSegmentBottom = coordinatesNewBottom[j].distance( coordinatesNewBottom[0] );
      // calculate distance between each top line point and the starting point of the top line
      final double distSegmentTop = coordinatesNewTop[j].distance( coordinatesNewTop[0] );

      // add a new point. the coordinates will be derived from the intersection nodes.
      meshPoints[j][0] = new Coordinate( coordinatesNewBottom[j] );

      for( int i = 0; i < coordinatesNewLeft.length; i++ ) 
      {
        final double dxLeftToRight = coordinatesNewRight[i].x - coordinatesNewLeft[i].x;
        final double dyLeftToRight = coordinatesNewRight[i].y - coordinatesNewLeft[i].y;

        final double ratio = i / (coordinatesNewLeft.length - 1);
        
        final double relativeSegmentDistance = distSegmentBottom * (1 - ratio) + distSegmentTop * ratio;
        final double relativeDistance = distBottom * (1 - ratio) + distTop * ratio;

        double x = coordinatesNewLeft[i].x + dxLeftToRight * relativeSegmentDistance / relativeDistance;
        double y = coordinatesNewLeft[i].y + dyLeftToRight * relativeSegmentDistance / relativeDistance;
        meshPoints[j][i] = new Coordinate( x, y );

      }

    }

    return meshPoints;

  }
}
