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

import junit.framework.TestCase;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * @author jung
 */
public class JTSQuadMesherTest extends TestCase
{

  /**
   * Test method for {@link org.kalypso.jts.JTSQuadMesher#calculateMesh()}.
   */
  public void testCalculateMesh( )
  {
    GeometryFactory factory = new GeometryFactory();

    Coordinate top1 = new Coordinate( 0, 3, 3 );
    Coordinate top2 = new Coordinate( 1, 3, 2 );
    Coordinate top3 = new Coordinate( 2, 3, 1 );
    Coordinate top4 = new Coordinate( 3, 3, 3 );
    Coordinate bottom1 = new Coordinate( 0, 0, 7 );
    Coordinate bottom2 = new Coordinate( 1, 0, 0 );
    Coordinate bottom3 = new Coordinate( 2, 0, 2 );
    Coordinate bottom4 = new Coordinate( 3, 0, 4 );
    Coordinate left1 = new Coordinate( 0.5, 1, 0 );
    Coordinate left2 = new Coordinate( 0.3, 2, 0 );
    Coordinate right1 = new Coordinate( 8, 1, 0 );
    Coordinate right2 = new Coordinate( 2.5, 2, 0 );

    Coordinate[] bottomCoordinates = new Coordinate[] { bottom1, bottom2, bottom3, bottom4 };
    Coordinate[] topCoordinates = new Coordinate[] { top1, top2, top3, top4 };
    Coordinate[] leftCoordinates = new Coordinate[] { bottom1, left1, left2, top1 };
    Coordinate[] rightCoordinates = new Coordinate[] { bottom4, right1, right2, top4 };

    LineString topLine = factory.createLineString( topCoordinates );
    LineString bottomLine = factory.createLineString( bottomCoordinates );
    LineString leftLine = factory.createLineString( leftCoordinates );
    LineString rightLine = factory.createLineString( rightCoordinates );

    JTSQuadMesher mesher = new JTSQuadMesher( topLine, bottomLine, leftLine, rightLine );

    Coordinate[][] test = mesher.calculateMesh();

    assertNotNull( test );
    assertEquals( 4, test.length );
    assertEquals( 4, test[0].length );
    assertEquals( 4, test[1].length );
    assertEquals( 4, test[2].length );

    for( int i = 0; i < test.length; i++ )
    {

      Coordinate[] coordinates = test[i];

//      for( int j = 0; j < coordinates.length; j++ )
//      {
//        Coordinate coordinate = coordinates[j];
//        assertEquals( (double) i, coordinate.x );
//        assertEquals( (double) j, coordinate.y );
//      }

    }
    JTSCoordsElevInterpol adjuster = new JTSCoordsElevInterpol( test );

    Coordinate[][] test2 = adjuster.calculateElevations();
    for( int i = 0; i < test2.length; i++ )
    {

      Coordinate[] coordinates = test2[i];

//      for( int j = 0; j < coordinates.length; j++ )
//      {
//        Coordinate coordinate = coordinates[j];
//        assertEquals( (double) i, coordinate.x );
//        assertEquals( (double) j, coordinate.y );
//      }

    }

    JTSCoordsAreaElevAdjust areaAdjuster = new JTSCoordsAreaElevAdjust( test2 );

    Coordinate[][] test3 = areaAdjuster.adjustElevations();
    for( int i = 0; i < test3.length; i++ )
    {

      Coordinate[] coordinates = test3[i];

//      for( int j = 0; j < coordinates.length; j++ )
//      {
//        Coordinate coordinate = coordinates[j];
//        assertEquals( (double) i, coordinate.x );
//        assertEquals( (double) j, coordinate.y );
//      }

    }
  }

}
