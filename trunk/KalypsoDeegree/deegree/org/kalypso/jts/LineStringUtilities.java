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

import java.util.LinkedList;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * utility to flip a lines string (change its node orientation)
 * 
 * input: line string
 * output: flipped line string
 * 
 * @author Thomas Jung
 *
 */
public class LineStringUtilities
{

  public static LineString changeOrientation( LineString InputLine )
  {
    // TODO Auto-generated method stub
 
    Coordinate[] coordinatesInputLine = InputLine.getCoordinates();
    LinkedList<Coordinate> newCoordinates = new LinkedList<Coordinate>(); 

    for (int i = coordinatesInputLine.length -1; i >=0; i++) 
    {
      newCoordinates.add( coordinatesInputLine[i] );
    }
    
    GeometryFactory factory = new GeometryFactory(InputLine.getPrecisionModel(), InputLine.getSRID());
    LineString flippedLine = factory.createLineString( newCoordinates.toArray( new Coordinate[]{} ) );

    return flippedLine;
  }

}
