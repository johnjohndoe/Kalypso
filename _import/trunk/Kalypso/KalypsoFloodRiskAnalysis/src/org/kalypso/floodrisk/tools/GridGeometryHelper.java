package org.kalypso.floodrisk.tools;

import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;
import org.opengis.cs.CS_CoordinateSystem;

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

public class GridGeometryHelper
{

  /**
   * controls, if coordinate system, origin, grid extent and offset is equal for
   * to given RectifiedGridDomains; If not, an Exception is thrown with a
   * certain errorMessage
   * 
   * @param gridDomain1
   * @param gridDomain2
   * @throws Exception
   */
  public static void controlGridGeometries( RectifiedGridDomain gridDomain1,
      RectifiedGridDomain gridDomain2 ) throws Exception
  {
    //check coordinateSystems
    CS_CoordinateSystem cs1 = gridDomain1.getOrigin( null ).getCoordinateSystem();
    CS_CoordinateSystem cs2 = gridDomain2.getOrigin( null ).getCoordinateSystem();
    if( !cs1.getName().equals( cs2.getName() ) )
    {
      String errorMessage = "Diffrent Coordinate System. (CS 1: " + cs1.getName() + ", CS 2: "
          + cs2.getName() + ")";
      System.out.println( "Error: " + errorMessage );
      throw new Exception( errorMessage );
    }

    //check origin
    GM_Point origin1 = gridDomain1.getOrigin( null );
    GM_Point origin2 = gridDomain2.getOrigin( null );
    // check x-Coordinate
    if( origin1.getX() != origin2.getX() )
    {
      String errorMessage = "Diffrent x-coordinate of origin. (x-coordinate 1: " + origin1.getX()
          + ", x-coordinate 2: " + origin2.getX() + ")";
      System.out.println( "Error: " + errorMessage );
      throw new Exception( errorMessage );
    }
    // check y-Coordinate
    if( origin1.getY() != origin2.getY() )
    {
      String errorMessage = "Diffrent y-coordinate of origin. (y-coordinate 1: " + origin1.getY()
          + ", y-coordinate 2: " + origin2.getY() + ")";
      System.out.println( "Error: " + errorMessage );
      throw new Exception( errorMessage );
    }

    // check extent
    int numCols1 = gridDomain1.getNumColumns();
    int numCols2 = gridDomain2.getNumColumns();
    int numRows1 = gridDomain1.getNumRows();
    int numRows2 = gridDomain2.getNumRows();
    // check number of columns
    if( numCols1 != numCols2 )
    {
      String errorMessage = "Diffrent number of columns. (Number of columns 1: " + numCols1
          + ", Number of columns 2: " + numCols2 + ")";
      System.out.println( "Error: " + errorMessage );
      throw new Exception( errorMessage );
    }
    //check number of rows
    if( numRows1 != numRows2 )
    {
      String errorMessage = "Diffrent number of rows. (Number of rows 1: " + numRows1
          + ", Number of rows 2: " + numRows2 + ")";
      System.out.println( "Error: " + errorMessage );
      throw new Exception( errorMessage );
    }
    // check offset
    double offsetX1 = gridDomain1.getOffsetX( cs1 );
    double offsetX2 = gridDomain2.getOffsetX( cs2 );
    double offsetY1 = gridDomain1.getOffsetY( cs1 );
    double offsetY2 = gridDomain2.getOffsetY( cs2 );
    // check offsetX
    if( offsetX1 != offsetX2 )
    {
      String errorMessage = "Diffrent offsetX. (OffsetX 1: " + offsetX1 + ", OffsetX 2: "
          + offsetX2 + ")";
      System.out.println( "Error: " + errorMessage );
      throw new Exception( errorMessage );
    }
    // check offsetY
    if( offsetY1 != offsetY2 )
    {
      String errorMessage = "Diffrent offsetY. (OffsetY 1: " + offsetY1 + ", OffsetY 2: "
          + offsetY2 + ")";
      System.out.println( "Error: " + errorMessage );
      throw new Exception( errorMessage );
    }
  }
}