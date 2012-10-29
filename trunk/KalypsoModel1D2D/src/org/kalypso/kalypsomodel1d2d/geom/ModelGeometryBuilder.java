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
package org.kalypso.kalypsomodel1d2d.geom;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypsodeegree.model.geometry.GM_CurveSegment;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_PolygonPatch;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Provide methods to build the geometry of the 1D 2D finite element model constituants like 2D element, continuity line
 * ...
 * 
 * @author Patrice Congo
 */
public class ModelGeometryBuilder
{
  /**
   * Create a surface given its exterior ring represented by a list of nodes
   * 
   * @param nodes
   *          the nodes representing the exterior of the surface
   * @return a surface which has its exterior specified by the nodes or null if less than 3 nodes have been provided
   * @throws IllegalArgumentException
   *           if nodes is null
   */
  public static final GM_PolygonPatch createSurfaceFromNode( final IFE1D2DNode[] nodes ) throws GM_Exception
  {
    final int nodeCount = nodes.length;

    /* Positions from nodes */
    final GM_Position[] poses = new GM_Position[nodeCount];

    final String crs = nodes[0].getPoint().getCoordinateSystem();
    for( int i = 0; i < nodeCount; i++ )
    {
      final GM_Point point = nodes[i].getPoint();
      poses[i] = point.getPosition();
    }

    return GeometryFactory.createGM_PolygonPatch( poses, new GM_Position[0][], crs );
  }

  public static final GM_CurveSegment computeEgdeGeometry( final IFE1D2DEdge edge )
  {
    // REMARK: we assume here, that all nodes live in the same coordinate
    // system.
    final IFE1D2DNode[] nodes = edge.getNodes();
    final int nodeCount = nodes.length;
    final IFE1D2DNode node0 = nodes[0];
    final GM_Point point0 = node0.getPoint();
    final String crs = point0.getCoordinateSystem();

    final GM_Position positions[] = new GM_Position[nodeCount];
    for( int i = 0; i < nodeCount; i++ )
    {
      final IFE1D2DNode nodei = nodes[i];
      final GM_Point point = nodei.getPoint();
      positions[i] = point.getPosition();
    }

    try
    {
      return GeometryFactory.createGM_CurveSegment( positions, crs );
    }
    catch( final GM_Exception e )
    {
      throw new IllegalStateException( e );
    }

    // FIXME: we must make sure that the envelope is updated when the location of the nodes
    // or the coordinate system has changed.
    // Sadly we cannot call edge.setEnvelopesUpdated();
    // here, as this calls getEnvelope immediately

    // This does not work, but is would be necessary
    // final GM_Envelope envelope = edge.getEnvelope();
    // final String envCrs = envelope.getCoordinateSystem();
    // final String curveCrs = curve.getCoordinateSystem();
    // if( curveCrs != null && !curveCrs.equals( envCrs ) )
    // edge.setEnvelopesUpdated();
  }
}
