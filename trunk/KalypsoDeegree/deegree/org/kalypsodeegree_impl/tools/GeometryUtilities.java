package org.kalypsodeegree_impl.tools;

import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

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

public class GeometryUtilities
{

  /*
   * 
   * @author doemming
   */
  public GeometryUtilities()
  {
    super();
  }

  public static GM_Curve createArrowLineString( GM_Point srcP, GM_Point targetP )
      throws GM_Exception
  {
    final GM_Position[] pos = new GM_Position[]
    {
        srcP.getPosition(),
        targetP.getPosition() };
    return GeometryFactory.createGM_Curve( pos, srcP.getCoordinateSystem() );
  }

  public static GM_Curve createArrowLineString( GM_Point srcP, GM_Point targetP,
      double weightLength, double weightWidth ) throws GM_Exception
  {
    double dx = targetP.getX() - srcP.getX();
    double dy = targetP.getY() - srcP.getY();

    final GM_Position p1 = srcP.getPosition();
    final GM_Position p4 = targetP.getPosition();
    final GM_Position p2 = GeometryFactory.createGM_Position( p1.getX() + weightLength * dx, p1
        .getY()
        + weightLength * dy );
    final GM_Position p3 = GeometryFactory.createGM_Position( p2.getX() + weightWidth * dy, p2
        .getY()
        - weightWidth * dx );
    final GM_Position p5 = GeometryFactory.createGM_Position( p2.getX() - weightWidth * dy, p2
        .getY()
        + weightWidth * dx );

    final GM_Position[] pos = new GM_Position[]
    {
        p1,
        p2,
        p3,
        p4,
        p5,
        p2 };
    return GeometryFactory.createGM_Curve( pos, srcP.getCoordinateSystem() );
  }

  /**
   * creates a new GM_Position that is on the straight line defined with the
   * positions and has a special distance from basePoint in the direction
   * towards the directionPoint
   */
  public static GM_Position createGM_PositionAt( GM_Position basePoint, GM_Position directionPoint,
      double distanceFromBasePoint )
  {
    final double[] p1 = basePoint.getAsArray();
    final double[] p2 = basePoint.getAsArray();
    final double factor=basePoint.getDistance(directionPoint)/distanceFromBasePoint;
    double newPos[]=new double[p1.length];
    for( int i = 0; i < newPos.length; i++ )
      newPos[i]=p1[i]+(p2[i]-p1[i])*factor;
    return GeometryFactory.createGM_Position(newPos);
  }

}