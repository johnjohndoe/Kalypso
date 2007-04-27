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
package org.kalypso.kalypsosimulationmodel.core;

import java.util.ArrayList;
import java.util.List;

import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Divide a surface into square cells.
 * The square cells are represented by an array of positions:
 * {@link GM_Position}[]{
 *      lower left corner, lower right corner, 
 *      upper right corner, upper left corner }
 * 
 * @author Patrice Congo
 */
public class SurfaceCellDivision
{
  
  public static final List<GM_Position[]> toCells(
                                GM_Surface surface,
                                ICellDivisionControl cellDivisionCheck)
  {
    Assert.throwIAEOnNullParam( surface, "surface" );
    Assert.throwIAEOnNullParam( cellDivisionCheck, "cellDivisionCheck" );
    
    List<GM_Position[]> resultCells = new ArrayList<GM_Position[]>();
    List<GM_Position[]> toCheckCells= new ArrayList<GM_Position[]>();
    
    addInitial( surface, toCheckCells ); 
    
    for( ;!toCheckCells.isEmpty(); )
    {
      final GM_Position[] positions = toCheckCells.remove( 0 );
      final int needDivision = 
              cellDivisionCheck.needDivision( positions );
      if( needDivision == ICellDivisionControl.DIVISION_TO_BE_CONTINUED )
      {
        divideCellInto4( positions, toCheckCells );
      }
      else if( needDivision == ICellDivisionControl.DIVISION_TO_BE_STOPED )
      {
        resultCells.add( positions );
      }
      else
      {
        //skip
      }
    }
    return resultCells;
    
  }
  
  private static final void addInitial(
                        GM_Surface surface, 
                        List<GM_Position[]> toCheckCells )
  {
    final GM_Envelope envelope = surface.getEnvelope();
    final GM_Position upperRight = envelope.getMax();
    final GM_Position lowerLeft = envelope.getMin();
    final GM_Position lowerRight = 
        GeometryFactory.createGM_Position( 
                    upperRight.getX(), lowerLeft.getY() );
    final GM_Position upperLeft = 
      GeometryFactory.createGM_Position( 
                  lowerLeft.getX(), upperRight.getY() );
    final GM_Position[] initial = 
            new GM_Position[]{lowerLeft, lowerRight, upperRight, upperLeft };
    divideCellInto4( initial, toCheckCells );
  }
  
  private static final void divideCellInto4(
                                GM_Position[] positions, 
                                List<GM_Position[]> toCheckCells )
  {
    final double minX = positions[0].getX();
    final double minY = positions[0].getX();
    
    final double maxX = positions[2].getX();    
    final double maxY = positions[2].getX();
    
    final double centerX = ( minX + maxX ) / 2.0;
    final double centerY = ( minY + maxY ) / 2.0;
    
    final GM_Position center = 
      GeometryFactory.createGM_Position( centerX, centerY );
    final GM_Position bottomSideCenter = 
      GeometryFactory.createGM_Position( centerX, minY );
    final GM_Position topSideCenter = 
      GeometryFactory.createGM_Position( centerX, maxY );
    final GM_Position leftSideCenter = 
      GeometryFactory.createGM_Position( minX, centerY );
    final GM_Position rightSideCenter = 
      GeometryFactory.createGM_Position( maxX, centerY );
    
    final GM_Position[] lowerLeftCell = 
      new GM_Position[]{positions[0], bottomSideCenter, center, leftSideCenter };
    toCheckCells.add( lowerLeftCell );
    
    final GM_Position[] lowerRightCell = 
      new GM_Position[]{ bottomSideCenter, positions[1], rightSideCenter, center };
    toCheckCells.add( lowerRightCell );
    
    final GM_Position[] upperRightCell = 
      new GM_Position[]{ center, rightSideCenter, positions[2], topSideCenter };
    toCheckCells.add( upperRightCell );
    
    final GM_Position[] upperLeftCell = 
      new GM_Position[]{leftSideCenter, center, topSideCenter, positions[3] };
    toCheckCells.add( upperLeftCell );
  }
  
  public static final double getCellArea(GM_Position[] cell)
  {
//    final double minX = cell[0].getX();
//    final double maxX = cell[1].getX();
    final double diffX = cell[1].getX() - cell[0].getX();
    final double diffY = cell[3].getY()- cell[0].getY();
    final double area = diffY * diffX;
    
    return area;
  }
  
  public static final GM_Position getCellCenter(GM_Position[] cell)
  {
    final double minX = cell[0].getX();
    final double maxX = cell[1].getX();
    final double minY = cell[0].getY();
    final double maxY = cell[3].getY();
    
    final double centerX = ( minX + maxX ) / 2.0;
    final double centerY = ( minY + maxY ) / 2.0;
    
    final GM_Position center = 
      GeometryFactory.createGM_Position( centerX, centerY );
    
    return center;
  }
  
}
