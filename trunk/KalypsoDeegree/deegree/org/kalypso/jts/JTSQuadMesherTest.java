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
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

import junit.framework.TestCase;

/**
 * @author jung
 *
 */
public class JTSQuadMesherTest extends TestCase
{

  /**
   * Test method for {@link org.kalypso.jts.JTSQuadMesher#calculateMesh()}.
   */
  public void testCalculateMesh( )
  {
    GeometryFactory factory = new GeometryFactory();
    
    Coordinate topLeft = new Coordinate(0,2);
    Coordinate topMiddle= new Coordinate(1,2);
    Coordinate topRight= new Coordinate(2,2);   
    Coordinate bottomLeft = new Coordinate(0,0);
    Coordinate bottomMiddle= new Coordinate(1,0);
    Coordinate bottomRight= new Coordinate(2,0);
    Coordinate middleLeft = new Coordinate(0,1);
    Coordinate middleRight= new Coordinate(2,1);
    
    Coordinate[] bottomCoordinates = new Coordinate[] { bottomLeft, bottomMiddle, bottomRight };
    Coordinate[] topCoordinates = new Coordinate[] { topLeft, topMiddle, topRight };
    Coordinate[] leftCoordinates = new Coordinate[] { bottomLeft, middleLeft, topLeft };
    Coordinate[] rightCoordinates = new Coordinate[] { bottomRight, middleRight, topRight };
    
    LineString topLine= factory.createLineString( topCoordinates );
    LineString bottomLine = factory.createLineString( bottomCoordinates );
    LineString leftLine = factory.createLineString( leftCoordinates );
    LineString rightLine = factory.createLineString( rightCoordinates );    
    
    JTSQuadMesher mesher = new JTSQuadMesher (topLine,bottomLine,leftLine,rightLine);
    Coordinate[][] test = mesher.calculateMesh();
    assertNotNull( test );
    assertEquals( 3, test.length );
    assertEquals( 3, test[0].length );
    assertEquals( 3, test[1].length );
    assertEquals( 3, test[2].length );
    
    for( int i = 0; i < test.length; i++ )
    {
      
      Coordinate[] coordinates = test[i];
      
      for( int j = 0; j < coordinates.length; j++ )
      {
        Coordinate coordinate = coordinates[j];
        assertEquals( (double)i, coordinate.x );
        assertEquals( (double)j, coordinate.y );
      }
      
    }
    
  }

}
