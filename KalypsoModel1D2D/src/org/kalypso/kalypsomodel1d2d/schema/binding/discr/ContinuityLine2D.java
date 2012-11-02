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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

public class ContinuityLine2D extends FELine implements IContinuityLine2D
{
  public ContinuityLine2D( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  private FeatureList nodesInternal( )
  {
    return (FeatureList)getProperty( PROP_NODES );
  }

  @Override
  public IFE1D2DNode[] getNodes( )
  {
    return nodesInternal().toFeatures( new IFE1D2DNode[nodesInternal().size()] );
  }

  @Override
  public void setNodes( final IFE1D2DNode[] nodes )
  {
    final List<IFE1D2DNode> recalculatedNodes = recalculateCurve( nodes );
    setGeometry( calculateGeometry( recalculatedNodes ) );
    final FeatureList nodesInternal = nodesInternal();
    nodesInternal.clear();
    for( final IFE1D2DNode node : recalculatedNodes )
      nodesInternal.addLink( node );
    setEnvelopesUpdated();
  }

  private GM_Curve calculateGeometry( final List<IFE1D2DNode> nodes )
  {
    final GM_Position[] nodePositions = new GM_Position[nodes.size()];
    for( int i = 0; i < nodePositions.length; i++ )
      nodePositions[i] = nodes.get( i ).getPoint().getPosition();

    String crs = nodes.get( 0 ).getPoint().getCoordinateSystem();

    if( crs == null )
      crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    try
    {
      return GeometryFactory.createGM_Curve( nodePositions, crs );
    }
    catch( final GM_Exception e )
    {
      throw new IllegalArgumentException( e );
    }
  }

  private final List<IFE1D2DNode> recalculateCurve( final IFE1D2DNode[] nodes )
  {
    final Iterator<IFE1D2DNode> iterator = Arrays.asList( nodes ).iterator();
    final IFE1D2DNode startNode = iterator.next();
    final List<IFE1D2DNode> curveNodes = new ArrayList<>();
    curveNodes.add( startNode );
    IFE1D2DNode currentNode = startNode;
    for( ; iterator.hasNext(); )
    {
      final IFE1D2DNode nextMilestoneNode = iterator.next();
      // TODO: !!!Potential endless loop!! I once got it (Gernot)
      while( !nextMilestoneNode.getId().equals( currentNode.getId() ) )
      {
        final Collection<IFE1D2DNode> neighbourNodes = currentNode.getAdjacentNodes();
        IFE1D2DNode bestCandidateNode = null;
        double shortestDistance = Double.MAX_VALUE;
        for( final IFE1D2DNode node : neighbourNodes )
        {
          final double nodesDistance = nodesDistance( node, nextMilestoneNode );
          if( nodesDistance < shortestDistance )
          {
            shortestDistance = nodesDistance;
            bestCandidateNode = node;
          }
        }
        currentNode = bestCandidateNode;
        curveNodes.add( currentNode );
      }
    }
    return curveNodes;
  }

  private double nodesDistance( final IFE1D2DNode node1, final IFE1D2DNode node2 )
  {
    final double x1 = node1.getPoint().getX();
    final double y1 = node1.getPoint().getY();
    final double x2 = node2.getPoint().getX();
    final double y2 = node2.getPoint().getY();
    return Math.sqrt( Math.pow( x1 - x2, 2 ) + Math.pow( y1 - y2, 2 ) );
  }
}
