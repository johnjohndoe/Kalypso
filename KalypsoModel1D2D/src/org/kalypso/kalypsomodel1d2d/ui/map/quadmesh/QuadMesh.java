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
package org.kalypso.kalypsomodel1d2d.ui.map.quadmesh;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.Assert;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_PolygonPatch;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * This class provide the mechanism to calculate the grid finit element model node and to display them. The point are
 * supposed to be in the same coordinate reference system so that no no reference system convertion is done
 * 
 * @author Patrice Congo
 * @author Thomas Jung
 */
public class QuadMesh
{
  /**
   * The target coordinate reference system for the created grid point
   */
  private final String m_crs;

  /**
   * Cache for grid computed grid points
   */
  private final Coordinate[][] m_grid;

  public QuadMesh( final Coordinate[][] grid, final String srsName )
  {
    Assert.isNotNull( grid );
    Assert.isNotNull( srsName );

    m_crs = srsName;
    m_grid = grid;

    for( final Coordinate[] line : grid )
    {
      Assert.isNotNull( line );

      for( final Coordinate point : line )
        Assert.isNotNull( point );
    }
  }

  public String getSRSName( )
  {
    return m_crs;
  }

  public List<GM_PolygonPatch> toRings( ) throws GM_Exception
  {
    final List<GM_PolygonPatch> rings = new ArrayList<>();

    for( int i = 0; i < m_grid.length - 1; i++ )
    {
      for( int j = 0; j < m_grid[i].length - 1; j++ )
      {
        final GM_Position[] poses = new GM_Position[5];

        poses[0] = JTSAdapter.wrap( m_grid[i][j] );
        poses[1] = JTSAdapter.wrap( m_grid[i + 1][j] );
        poses[2] = JTSAdapter.wrap( m_grid[i + 1][j + 1] );
        poses[3] = JTSAdapter.wrap( m_grid[i][j + 1] );
        poses[4] = JTSAdapter.wrap( m_grid[i][j] );

        // final GM_Position[] checkedPoses = checkPoses( poses, searchRectWidth );
        final GM_Position[] checkedPoses = poses;

        if( checkedPoses.length >= 4 && checkedPoses[0].equals( checkedPoses[checkedPoses.length - 1] ) )
          rings.add( GeometryFactory.createGM_PolygonPatch( checkedPoses, null, m_crs ) );
      }
    }

    return Collections.unmodifiableList( rings );
  }

//  // TODO: seems to delete consecutive that are too near to each other -> necessary? There are better ways to do that
//  private GM_Position[] checkPoses( final GM_Position[] poses, final double searchRectWidth )
//  {
//    final List<GM_Position> posToDeleteList = new ArrayList<GM_Position>();
//    final List<GM_Position> posList = new ArrayList<GM_Position>();
//
//    for( int i = 0; i < poses.length - 1; i++ )
//    {
//      posList.add( poses[i] );
//
//      /* check the distance to each other */
//      for( int j = 0; j < poses.length - 1; j++ )
//      {
//        if( i != j )
//        {
//          // TODO: what is the meaning of this?
//
//          final double distance = poses[i].getDistance( poses[j] );
//          if( distance < 2 * searchRectWidth && !posToDeleteList.contains( poses[j] ) )
//          {
//            posToDeleteList.add( poses[i] );
//          }
//        }
//      }
//    }
//
//    for( final GM_Position position : posToDeleteList )
//    {
//      posList.remove( position );
//    }
//
//    final GM_Position[] positions = posList.toArray( new GM_Position[posList.size() + 1] );
//    positions[posList.size()] = positions[0];
//    return positions;
//  }

  public Coordinate[][] getGrid( )
  {
    return m_grid;
  }
}