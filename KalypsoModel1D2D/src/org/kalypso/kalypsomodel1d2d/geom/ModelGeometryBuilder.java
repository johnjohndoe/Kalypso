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

import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Provide methods to build the geometry of the 1D 2D finite element model constituants like 2D element, continuity line
 * ...
 * 
 * @author Patrice Congo
 * 
 */
public class ModelGeometryBuilder
{

  /**
   * Create a surface given its exterior ring represented by a list of nodes
   * 
   * @param nodes
   *          the nodes representing the exterior of the surface
   * @return a surface which has its exterior specified by the nodes or null if less than 3 nodes have been provided
   * 
   * @throws IllegalArgumentException
   *           if nodes is null
   */
  public static final GM_Surface createSurfaceFromNode( final List<IFE1D2DNode> nodes ) throws GM_Exception
  {
    Assert.throwIAEOnNullParam( nodes, "nodes" ); //$NON-NLS-1$

    final int SIZE = nodes.size();
    /* Positions from nodes */
    final GM_Position[] poses = new GM_Position[SIZE];

    if( SIZE <= 3 )
      return null;

    String crs = nodes.get( 0 ).getPoint().getCoordinateSystem();
    if( crs == null )
      crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    for( int i = 0; i < poses.length; i++ )
    {
      final GM_Point point = nodes.get( i ).getPoint();
      poses[i] = point.getPosition();
    }

    return GeometryFactory.createGM_Surface( poses, new GM_Position[0][], crs );
  }

  public static final GM_Curve computeEgdeGeometry( final IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge ) throws GM_Exception
  {
    // REMARK: we assume here, that all nodes live in the same coordinate
    // system.
    if( edge == null )
      return null;

    final List<IFE1D2DNode> nodes = edge.getNodes();
    final int SIZE = nodes.size();

    if( SIZE != 2 )
      return null;

    final IFE1D2DNode node0 = nodes.get( 0 );
    if( node0 == null )
      return null;

    final GM_Point point0 = node0.getPoint();
    final String crs = point0.getCoordinateSystem();

    final GM_Position positions[] = new GM_Position[SIZE];
    for( int i = 0; i < SIZE; i++ )
    {
      final IFE1D2DNode nodei = nodes.get( i );
      if( nodei == null )
        return null;

      final GM_Point point = nodei.getPoint();
      positions[i] = point.getPosition();
    }

    final GM_Curve curve = GeometryFactory.createGM_Curve( positions, crs );

    // FIXME: we must make sure that the envelope is updated when the location of the nodes
    // or the coordinate system has changed.
    // Sadly we cannot call edge.getFeature().setEnvelopesUpdated();
    // here, as this calls getEnvelope immediately

    // This does not work, but is would be necessary
    // final GM_Envelope envelope = edge.getFeature().getEnvelope();
    // final String envCrs = envelope.getCoordinateSystem();
    // final String curveCrs = curve.getCoordinateSystem();
    // if( curveCrs != null && !curveCrs.equals( envCrs ) )
    // edge.getFeature().setEnvelopesUpdated();

    return curve;
  }
}
