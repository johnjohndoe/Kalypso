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

import java.awt.Graphics;

import org.kalypso.ogc.gml.map.widgets.advanced.utils.SLDPainter2;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Gernot Belger
 */
public class QuadMeshPainter
{
  private final QuadMesh m_mesh;

  private final SLDPainter2 m_edgePainter;

  private final SLDPainter2 m_vertexPainter;

  public QuadMeshPainter( final QuadMesh mesh, final SLDPainter2 edgePainter, final SLDPainter2 vertexPainter )
  {
    m_mesh = mesh;
    m_edgePainter = edgePainter;
    m_vertexPainter = vertexPainter;
  }

  public void paint( final Graphics g, final GeoTransform projection )
  {
    try
    {
      if( m_mesh == null )
        return;

      if( m_edgePainter != null )
        paintEdges( g, projection );

      if( m_vertexPainter != null )
        paintVertices( g, projection );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
  }

  private void paintEdges( final Graphics g, final GeoTransform projection ) throws GM_Exception
  {
    final String srsName = m_mesh.getSRSName();

    final Coordinate[][] grid = m_mesh.getGrid();

    /* paint horizontal lines */
    for( final Coordinate[] line : grid )
    {
      for( int j = 0; j < line.length - 1; j++ )
      {
        final GM_Position pos0 = JTSAdapter.wrap( line[j] );
        final GM_Position pos1 = JTSAdapter.wrap( line[j + 1] );

        final GM_Curve curve = GeometryFactory.createGM_Curve( new GM_Position[] { pos0, pos1 }, srsName );
        m_edgePainter.paint( g, projection, curve );
      }
    }

    /* paint vertical lines */
    for( int j = 0; j < grid[0].length; j++ )
    {
      for( int i = 0; i < grid.length - 1; i++ )
      {
        final GM_Position pos0 = JTSAdapter.wrap( grid[i][j] );
        final GM_Position pos1 = JTSAdapter.wrap( grid[i + 1][j] );

        final GM_Curve curve = GeometryFactory.createGM_Curve( new GM_Position[] { pos0, pos1 }, srsName );

        m_edgePainter.paint( g, projection, curve );
      }
    }
  }

  private void paintVertices( final Graphics g, final GeoTransform projection )
  {
    final String srsName = m_mesh.getSRSName();
    final Coordinate[][] grid = m_mesh.getGrid();

    /* paint horizontal lines */
    for( final Coordinate[] line : grid )
    {
      for( final Coordinate vertex : line )
      {
        final GM_Point point = GeometryFactory.createGM_Point( vertex.x, vertex.y, srsName );
        m_vertexPainter.paint( g, projection, point );
      }
    }
  }
}