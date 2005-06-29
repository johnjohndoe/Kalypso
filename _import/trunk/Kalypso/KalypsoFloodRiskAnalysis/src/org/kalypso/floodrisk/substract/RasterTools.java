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
package org.kalypso.floodrisk.substract;

import java.util.Vector;

import org.kalypso.floodrisk.tools.GridGeometryHelper;
import org.kalypsodeegree_impl.model.cv.RangeSet;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;

/**
 * 
 * RasterTools
 * <p>
 * 
 * created by
 * 
 * @author Nadja Peiler (17.06.2005)
 */
public class RasterTools
{

  /**
   * substracts two grids (grid1 - grid2), checks each gridcell, when in a gridcell of grid1 a value is stored and in
   * the same gridcell of grid2 no data value is stored, then the value of the result grid for this cell is 1; otherwise
   * it is 0
   * 
   * @param grid1
   * @param grid2
   * @return resultGrid Grid with only cellValues 0 or 1
   */
  public static RectifiedGridCoverage substractGrids( RectifiedGridCoverage grid1, RectifiedGridCoverage grid2 )
      throws Exception
  {
    // control Geometries
    GridGeometryHelper.controlGridGeometries( grid1.getGridDomain(), grid2.getGridDomain() );

    RectifiedGridDomain resultGridDomain = new RectifiedGridDomain( grid1.getGridDomain().getOrigin( null ), grid1
        .getGridDomain().getOffset(), grid1.getGridDomain().getGridRange() );
    Vector resultRangeSetData = new Vector();
    Vector grid1RangeSetData = grid1.getRangeSet().getRangeSetData();
    Vector grid2RangeSetData = grid2.getRangeSet().getRangeSetData();
    for( int i = 0; i < grid1RangeSetData.size(); i++ )
    {
      Vector grid1_rowData = (Vector)grid1RangeSetData.get( i );
      Vector grid2_rowData = (Vector)grid2RangeSetData.get( i );
      Vector result_rowData = new Vector();
      for( int j = 0; j < grid1_rowData.size(); j++ )
      {
        if( grid1_rowData.get( j ) != null )
        {
          if( grid2_rowData.get( j ) != null )
          {
            result_rowData.addElement( new Double( 0 ) );
          }
          else
          {
            result_rowData.addElement( new Double( 1 ) );
          }
        }
        else
        {
          result_rowData.addElement( null );
        }
      }//for j (Spalten)
      resultRangeSetData.addElement( result_rowData );
    }//for i (Zeilen)
    RangeSet resultRangeSet = new RangeSet( resultRangeSetData, null );
    return new RectifiedGridCoverage( resultGridDomain, resultRangeSet );
  }

}