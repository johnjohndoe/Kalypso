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
package org.kalypso.floodrisk.tools;

import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;

/**
 * 
 * GridGeometryHelper
 * <p>
 * Holds methods for controling the geometry of a grid
 * 
 * created by
 * 
 * @author Nadja Peiler (17.06.2005)
 */

public class GridGeometryHelper
{

  /**
   * controls, if coordinate system, origin, grid extent and offset is equal for two given RectifiedGridDomains; If not,
   * an Exception is thrown with a certain errorMessage
   * 
   * @param gridDomain1
   * @param gridDomain2
   * @throws Exception
   */
  public static void controlGridGeometries( RectifiedGridDomain gridDomain1, RectifiedGridDomain gridDomain2 )
      throws Exception
  {
    // TODO: write equals method on RectifiedGridDomain 
    
//    //check coordinateSystems
//    CS_CoordinateSystem cs1 = gridDomain1.getOrigin( null ).getCoordinateSystem();
//    CS_CoordinateSystem cs2 = gridDomain2.getOrigin( null ).getCoordinateSystem();
//    if( !cs1.getName().equals( cs2.getName() ) )
//    {
//      String errorMessage = Messages.getString("tools.GridGeometryHelper.DifferentCoordinateSystems")+". (CS 1: " + cs1.getName() + ", CS 2: " + cs2.getName() + ")"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
//      System.out.println( Messages.getString("tools.GridGeometryHelper.Error")+": " + errorMessage ); //$NON-NLS-1$ //$NON-NLS-2$
//      throw new Exception( errorMessage );
//    }
//
//    //check origin
//    GM_Point origin1 = gridDomain1.getOrigin( null );
//    GM_Point origin2 = gridDomain2.getOrigin( null );
//    // check x-Coordinate
//    if( origin1.getX() != origin2.getX() )
//    {
//      String errorMessage = Messages.getString("tools.GridGeometryHelper.DifferentXcoordinateOfOrigin")+". ("+Messages.getString("tools.GridGeometryHelper.Xcoordinate")+" 1: " + origin1.getX() //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
//          + ", "+Messages.getString("tools.GridGeometryHelper.Xcoordinate")+" 2"+": " + origin2.getX() + ")"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
//      System.out.println( Messages.getString("tools.GridGeometryHelper.Error")+": " + errorMessage ); //$NON-NLS-1$ //$NON-NLS-2$
//      throw new Exception( errorMessage );
//    }
//    // check y-Coordinate
//    if( origin1.getY() != origin2.getY() )
//    {
//      String errorMessage = Messages.getString("tools.GridGeometryHelper.DifferentYcoordinateOfOrigin")+". ("+Messages.getString("tools.GridGeometryHelper.Ycoordinate")+" 1: " + origin1.getY() //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
//          + ", "+Messages.getString("tools.GridGeometryHelper.Ycoordinate")+" 2: " + origin2.getY() + ")"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
//      System.out.println( Messages.getString("tools.GridGeometryHelper.Error")+": " + errorMessage ); //$NON-NLS-1$ //$NON-NLS-2$
//      throw new Exception( errorMessage );
//    }
//
//    // check extent
//    int numCols1 = gridDomain1.getNumColumns();
//    int numCols2 = gridDomain2.getNumColumns();
//    int numRows1 = gridDomain1.getNumRows();
//    int numRows2 = gridDomain2.getNumRows();
//    // check number of columns
//    if( numCols1 != numCols2 )
//    {
//      String errorMessage = Messages.getString("tools.GridGeometryHelper.DifferentNumberOfColumns")+". ("+Messages.getString("tools.GridGeometryHelper.NumberOfColumns")+" 1: " + numCols1 + ", "+Messages.getString("tools.GridGeometryHelper.NumberOfColumns")+" 2: " //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
//          + numCols2 + ")"; //$NON-NLS-1$
//      System.out.println( Messages.getString("tools.GridGeometryHelper.Error")+": " + errorMessage ); //$NON-NLS-1$ //$NON-NLS-2$
//      throw new Exception( errorMessage );
//    }
//    //check number of rows
//    if( numRows1 != numRows2 )
//    {
//      String errorMessage = Messages.getString("tools.GridGeometryHelper.DifferentNumberOfRows")+". ("+Messages.getString("tools.GridGeometryHelper.NumberOfRows")+" 1: " + numRows1 + ", "+Messages.getString("tools.GridGeometryHelper.NumberOfRows")+" 2: " //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
//          + numRows2 + ")"; //$NON-NLS-1$
//      System.out.println( Messages.getString("tools.GridGeometryHelper.Error")+": " + errorMessage ); //$NON-NLS-1$ //$NON-NLS-2$
//      throw new Exception( errorMessage );
//    }
//    // check offset
//    double offsetX1 = gridDomain1.getOffsetX( cs1 );
//    double offsetX2 = gridDomain2.getOffsetX( cs2 );
//    double offsetY1 = gridDomain1.getOffsetY( cs1 );
//    double offsetY2 = gridDomain2.getOffsetY( cs2 );
//    // check offsetX
//    if( offsetX1 != offsetX2 )
//    {
//      String errorMessage = Messages.getString("tools.GridGeometryHelper.DifferentOffsetX")+". ("+Messages.getString("tools.GridGeometryHelper.OffsetX")+" 1: " + offsetX1 + ", "+Messages.getString("tools.GridGeometryHelper.OffsetX")+" 2: " + offsetX2 + ")"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
//      System.out.println( Messages.getString("tools.GridGeometryHelper.Error")+": " + errorMessage ); //$NON-NLS-1$ //$NON-NLS-2$
//      throw new Exception( errorMessage );
//    }
//    // check offsetY
//    if( offsetY1 != offsetY2 )
//    {
//      String errorMessage = Messages.getString("tools.GridGeometryHelper.DifferentOffsetY")+". ("+Messages.getString("tools.GridGeometryHelper.OffsetY")+" 1: " + offsetY1 + ", "+Messages.getString("tools.GridGeometryHelper.OffsetY")+" 2: " + offsetY2 + ")"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
//      System.out.println( Messages.getString("tools.GridGeometryHelper.Error")+": " + errorMessage ); //$NON-NLS-1$ //$NON-NLS-2$
//      throw new Exception( errorMessage );
//    }
  }
}