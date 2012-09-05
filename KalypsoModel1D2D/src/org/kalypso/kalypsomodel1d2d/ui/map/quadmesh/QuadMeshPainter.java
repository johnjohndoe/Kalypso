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

/**
 * @author Gernot Belger
 */
public class QuadMeshPainter
{
  private final QuadMesh m_mesh;

  private final SLDPainter2 m_edgePainter;

  public QuadMeshPainter( final QuadMesh mesh, final SLDPainter2 edgePainter )
  {
    m_mesh = mesh;
    m_edgePainter = edgePainter;
  }

  public void paint( final Graphics g, final GeoTransform projection )
  {
    try
    {
      if( m_mesh == null )
        return;

      paintEdges( g, projection );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
  }

  private void paintEdges( final Graphics g, final GeoTransform projection ) throws GM_Exception
  {
    final GM_Point[][] points = m_mesh.getGrid();

    /* paint horizontal lines */
    for( final GM_Point[] line : points )
    {
      for( int j = 0; j < line.length - 1; j++ )
      {
        final GM_Position pos0 = line[j].getPosition();
        final GM_Position pos1 = line[j + 1].getPosition();

        final GM_Curve curve = GeometryFactory.createGM_Curve( new GM_Position[] { pos0, pos1 }, line[j].getCoordinateSystem() );
        m_edgePainter.paint( g, projection, curve );
      }
    }

    /* paint vertical lines */
    for( int j = 0; j < points[0].length; j++ )
    {
      for( int i = 0; i < points.length - 1; i++ )
      {
        final GM_Point point0 = points[i][j];

        final GM_Position pos0 = point0.getPosition();
        final GM_Position pos1 = points[i + 1][j].getPosition();

        final GM_Curve curve = GeometryFactory.createGM_Curve( new GM_Position[] { pos0, pos1 }, point0.getCoordinateSystem() );

        m_edgePainter.paint( g, projection, curve );
      }
    }
  }
}