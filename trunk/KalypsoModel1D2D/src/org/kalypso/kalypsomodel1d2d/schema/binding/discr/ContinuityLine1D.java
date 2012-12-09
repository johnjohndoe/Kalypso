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
package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

public class ContinuityLine1D extends FELine implements IContinuityLine1D
{
  public ContinuityLine1D( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public IFE1D2DNode[] getNodes( )
  {
    return new IFE1D2DNode[] { getNode() };
  }

  @Override
  public IFE1D2DNode getNode( )
  {
    return (IFE1D2DNode)resolveMember( PROP_NODES );
  }

  @Override
  public void setNode( final IFE1D2DNode node )
  {
    Assert.throwIAEOnNullParam( node, "node" ); //$NON-NLS-1$

    final GM_Curve curve = calculateGeometry( node );
    setGeometry( curve );

    setLink( PROP_NODES, node );

    setEnvelopesUpdated();
  }

  public static GM_Curve calculateGeometry( final IFE1D2DNode node )
  {
    final IFE1D2DNode neighbour = node.getAdjacentNodes().get( 0 );

    final GM_Point p1 = node.getPoint();
    final GM_Point p2 = neighbour.getPoint();
    final double x1 = p1.getX() - (p2.getY() - p1.getY());
    final double y1 = p1.getY() + (p2.getX() - p1.getX());
    final double x2 = p1.getX() + (p2.getY() - p1.getY());
    final double y2 = p1.getY() - (p2.getX() - p1.getX());
    final GM_Position[] positions = new GM_Position[] { GeometryFactory.createGM_Position( x1, y1 ), GeometryFactory.createGM_Position( x2, y2 ) };
    try
    {
      return GeometryFactory.createGM_Curve( positions, p1.getCoordinateSystem() );
    }
    catch( final GM_Exception e )
    {
      throw new IllegalArgumentException( e );
    }
  }
}
