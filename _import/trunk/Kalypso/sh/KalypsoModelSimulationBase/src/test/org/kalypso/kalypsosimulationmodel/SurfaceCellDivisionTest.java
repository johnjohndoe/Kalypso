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
package test.org.kalypso.kalypsosimulationmodel;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import junit.framework.TestCase;

import org.kalypso.kalypsosimulationmodel.core.ICellDivisionControl;
import org.kalypso.kalypsosimulationmodel.core.SurfaceCellDivision;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Test for {@link SurfaceCellDivision}
 * 
 * @author Patrice Control
 *
 */
public class SurfaceCellDivisionTest extends TestCase
{
  public void testMinimalQuadDivision()
  {
    try
    {
      final ICellDivisionControl stopDivisionControl =
        new ICellDivisionControl()
      {
  
        public int needDivision( GM_Position[] positions )
        {
          return ICellDivisionControl.DIVISION_TO_BE_STOPED;
        }      
      };
      
      final double[] exteriorRP1 = { 0,0,0, 0,1,0, 1,1,0, 1,0,0, 0,0,0 };
      final GM_Surface surface = makeSurface( exteriorRP1 );
      
      List<GM_Position[]> cells = SurfaceCellDivision.toCells( surface, stopDivisionControl );
      //converted since compare works with lists
      List< List<GM_Position>> cellsAsLists = 
          new ArrayList<List<GM_Position>>(cells.size());
      for( GM_Position[] cell : cells )
      {
        cellsAsLists.add(  Arrays.asList( cell ) );
      }
      
      assertEquals( 
          "4 must be the size since stop control returns false", 
          4,
          cells.size() );
      final GM_Position[] lowerLeftCell = makeCell( 0.0, 0.5, 0.0, 0.5);
      final GM_Position[] lowerRightCell = makeCell( 0.5, 1, 0.0, 0.5);
      final GM_Position[] upperLeftCell = makeCell( 0.0, 0.5, 0.5, 1);
      final GM_Position[] upperRightCell = makeCell( 0.5, 1, 0.5, 1);
      assertTrue( 
          "lowerLeft must be in the cells list returned", 
          cellsAsLists.contains( 
                  Arrays.asList( lowerLeftCell ) ) );
      assertTrue( 
          "lowerRight must be in the cells list returned", 
          cellsAsLists.contains( 
                  Arrays.asList( lowerRightCell ) ) );
      assertTrue( 
          "upperLeft must be in the cells list returned", 
          cellsAsLists.contains( 
                  Arrays.asList( upperLeftCell ) ) );
      assertTrue( 
          "upperRight must be in the cells list returned", 
          cellsAsLists.contains( 
                  Arrays.asList( upperRightCell ) ) );
    }
    catch( Throwable th )
    {
      fail( TestUtils.getStackTraceAsString( th ) );
    }
    
  }
  
  private GM_Position[] makeCell( 
              double minX, double maxX, 
              double minY, double maxY )
  {
    final GM_Position lowerLeft = 
      GeometryFactory.createGM_Position( minX, minY );
    final GM_Position lowerRight = 
      GeometryFactory.createGM_Position( maxX, minY );
    final GM_Position upperLeft = 
      GeometryFactory.createGM_Position( minX, maxY );
    final GM_Position upperRight = 
      GeometryFactory.createGM_Position( maxX, maxY );
    
    return  
        new GM_Position[]{
              lowerLeft, lowerRight, upperRight, upperLeft };
  }
  
  private static final GM_Surface makeSurface( double[] exterior ) throws GM_Exception
  {
    GM_Surface surfacePatch = 
      GeometryFactory.createGM_Surface( 
            exterior, 
            TestWorkspaces.NO_INTERIOR, 
            3, 
            TestWorkspaces.getGaussKrueger());
    return surfacePatch;
  }
  
}
