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
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

public class ContinuityLine2D extends FELine implements IContinuityLine2D
{
  public ContinuityLine2D( final Feature featureToBind )
  {
    this( featureToBind, IContinuityLine2D.QNAME );
  }

  public ContinuityLine2D( final Feature featureToBind, final QName featureQName )
  {
    super( featureToBind, featureQName );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.FELine#createFullNodesList(java.util.List)
   */
  @Override
  public List<IFE1D2DNode> createFullNodesList( final List<IFE1D2DNode> nodes ) throws CoreException
  {
    if( nodes.size() < 2 )
      throw new CoreException( StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.discr.ContinuityLine2D.0" ) ) ); //$NON-NLS-1$
    final List<IFE1D2DNode> fullNodeList;
    try
    {
      fullNodeList = recalculateGeometry( nodes );
    }
    catch( final GM_Exception e )
    {
      throw new CoreException( StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.discr.ContinuityLine2D.1" ) + e.getLocalizedMessage() ) ); //$NON-NLS-1$
    }
    final FeatureList nodeList = (FeatureList) getFeature().getProperty( IFELine.PROP_NODES );
    nodeList.clear();
    for( final IFE1D2DNode node : fullNodeList )
      nodeList.add( node.getFeature().getId() );
    nodeList.invalidate();
    getFeature().invalidEnvelope();
    return m_nodes;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem#recalculateElementGeometry()
   */
  @Override
  public GM_Object recalculateElementGeometry( ) throws GM_Exception
  {
    try
    {
      final ArrayList<IFE1D2DNode> list = new ArrayList<IFE1D2DNode>();
      list.addAll( getNodes() );
      recalculateGeometry( list );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
    return (GM_Object) getFeature().getProperty( IFELine.PROP_GEOMETRY );
  }

  private List<IFE1D2DNode> recalculateGeometry( final List<IFE1D2DNode> nodes ) throws GM_Exception, CoreException
  {
    final List<IFE1D2DNode> recalculatedNodes = recalculateCurve( nodes );
    final GM_Position[] nodePositions = new GM_Position[recalculatedNodes.size()];
    for( int i = 0; i < nodePositions.length; i++ )
      nodePositions[i] = recalculatedNodes.get( i ).getPoint().getPosition();
    String crs = nodes.get( 0 ).getPoint().getCoordinateSystem();
    if( crs == null )
      crs = KalypsoCorePlugin.getDefault().getCoordinatesSystem();
    setGeometry( GeometryFactory.createGM_Curve( nodePositions, crs ) );
    return recalculatedNodes;
  }

  private final List<IFE1D2DNode> recalculateCurve( final List<IFE1D2DNode> nodes ) throws CoreException
  {
    final Iterator<IFE1D2DNode> iterator = nodes.iterator();
    final IFE1D2DNode startNode = iterator.next();
    final IFEDiscretisationModel1d2d model = new FE1D2DDiscretisationModel( startNode.getFeature().getWorkspace().getRootFeature() );
    final List<IFE1D2DNode> curveNodes = new ArrayList<IFE1D2DNode>();
    curveNodes.add( startNode );
    IFE1D2DNode currentNode = startNode;
    for( ; iterator.hasNext(); )
    {
      final IFE1D2DNode nextMilestoneNode = iterator.next();
      // TODO: !!!Potential endless loop!! I once got it (Gernot)
      while( !nextMilestoneNode.getGmlID().equals( currentNode.getGmlID() ) )
      {
        final Collection<IFE1D2DNode> neighbourNodes = currentNode.getNeighbours();
        if( neighbourNodes.size() == 0 )
        {
          final IStatus status = StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.discr.ContinuityLine2D.2" ) + currentNode.getFeature().getId() ); //$NON-NLS-1$
          throw new CoreException( status );
        }
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
