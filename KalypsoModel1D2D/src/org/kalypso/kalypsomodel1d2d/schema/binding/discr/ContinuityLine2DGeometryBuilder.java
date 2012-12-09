/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Calculates a continuity line from a list of nodes.
 * 
 * @author Gernot Belger
 */
public class ContinuityLine2DGeometryBuilder
{
  private final IFE1D2DNode[] m_continuityNodes;

  public ContinuityLine2DGeometryBuilder( final IFE1D2DNode[] nodes, final IProgressMonitor monitor )
  {
    m_continuityNodes = recalculateContinuityLine( nodes, monitor );
  }

  public GM_Curve getCurve( )
  {
    if( m_continuityNodes.length < 2 )
      return null;

    final GM_Position[] nodePositions = new GM_Position[m_continuityNodes.length];
    for( int i = 0; i < nodePositions.length; i++ )
      nodePositions[i] = m_continuityNodes[i].getPoint().getPosition();

    try
    {
      final String crs = m_continuityNodes[0].getPoint().getCoordinateSystem();
      if( crs == null )
        throw new IllegalStateException();

      return GeometryFactory.createGM_Curve( nodePositions, crs );
    }
    catch( final GM_Exception e )
    {
      throw new IllegalArgumentException( e );
    }
  }

  public IFE1D2DNode[] getContinuityNodes( )
  {
    return m_continuityNodes;
  }

  private IFE1D2DNode[] recalculateContinuityLine( final IFE1D2DNode[] nodes, final IProgressMonitor monitor )
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

          ProgressUtilities.worked( monitor, 1 );
        }
        currentNode = bestCandidateNode;
        curveNodes.add( currentNode );

        ProgressUtilities.worked( monitor, 1 );
      }
    }

    return curveNodes.toArray( new IFE1D2DNode[curveNodes.size()] );
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