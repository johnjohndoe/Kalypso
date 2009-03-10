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
package org.kalypso.kalypsomodel1d2d.conv;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv.RESULTLINES;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * The handler that converts the RMA∑Kalypso element events into discretisation model elements and links
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 * @author Patrice Congo
 * 
 */
public class DiscretisationModel1d2dHandler implements IRMA10SModelElementHandler
{
  /**
   * The model to fill with the parsed fe element from
   */
  private final IFEDiscretisationModel1d2d m_model;

  private final List<IFeatureWrapper2> m_createdFeatures = new ArrayList<IFeatureWrapper2>();

  private final GMLWorkspace m_workspace;

  private final IPositionProvider m_positionProvider;

  private final HashMap<Integer, String> m_nodesNameConversionMap = new HashMap<Integer, String>( 10000 );

  private final HashMap<Integer, String> m_edgesNameConversionMap = new HashMap<Integer, String>( 50000 );

  private final HashMap<Integer, String> m_elementsNameConversionMap = new HashMap<Integer, String>( 5000 );

  private final HashMap<GM_Point, IFE1D2DNode> m_pointCashe = new HashMap<GM_Point, IFE1D2DNode>( 10000 );

  public DiscretisationModel1d2dHandler( final IFEDiscretisationModel1d2d model, final IPositionProvider positionProvider )
  {
    m_model = model;
    m_workspace = model.getFeature().getWorkspace();
    m_positionProvider = positionProvider;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#start()
   */
  public void start( )
  {
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#end()
   */
  public void end( )
  {
    // for( final IFE1D2DElement elelemt : m_model.getElements() )
    // ModelOps.sortElementEdges( elelemt );
    m_model.getElements().getWrappedList().invalidate();
    m_model.getEdges().getWrappedList().invalidate();
    m_model.getNodes().getWrappedList().invalidate();
    final int size = m_createdFeatures.size();
    final Feature[] createdFeatures = new Feature[size];
    for( int i = size - 1; i >= 0; i-- )
    {
      createdFeatures[i] = m_createdFeatures.get( i ).getFeature();
      createdFeatures[i].invalidEnvelope();
    }
    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_model.getFeature(), createdFeatures, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
    // workspace.fireModellEvent( new )
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleArc(java.lang.String, int, int, int, int,
   *      int, int)
   */
  public void handleArc( final String lineString, final int id, final int node1ID, final int node2ID, final int elementLeftID, final int elementRightID, final int middleNodeID )
  {
    final boolean useMiddleNode = middleNodeID > 0;
    final String edgeGmlID = m_edgesNameConversionMap.get( id );
    final IFE1D2DEdge edge;
    final Feature edgeFeature = m_workspace.getFeature( edgeGmlID );
    final IFE1D2DNode node1 = getNode( node1ID );
    final IFE1D2DNode node2 = getNode( node2ID );
    final IFE1D2DNode middleNode = useMiddleNode ? getNode( middleNodeID ) : null;
    if( node1 == null )
      throw new RuntimeException( String.format( Messages.getString("org.kalypso.kalypsomodel1d2d.conv.DiscretisationModel1d2dHandler.0"), node1ID,id)); //$NON-NLS-1$
    if( node2 == null )
      throw new RuntimeException( String.format( Messages.getString("org.kalypso.kalypsomodel1d2d.conv.DiscretisationModel1d2dHandler.1"), node1ID,id)); //$NON-NLS-1$
    if( useMiddleNode && middleNode == null )
      throw new RuntimeException( String.format( Messages.getString("org.kalypso.kalypsomodel1d2d.conv.DiscretisationModel1d2dHandler.2"), node1ID,id)); //$NON-NLS-1$
    if( edgeFeature != null )
      edge = (IFE1D2DEdge) edgeFeature.getAdapter( IFE1D2DEdge.class );
    else
    {
      edge = m_model.getEdges().addNew( IFE1D2DEdge.QNAME );
      m_createdFeatures.add( edge );

      final String newEdgeGmlID = edge.getGmlID();
      m_edgesNameConversionMap.put( id, newEdgeGmlID );
      edge.addNode( node1.getGmlID() );
      edge.addNode( node2.getGmlID() );
      if( useMiddleNode )
        edge.setMiddleNode( middleNode );
      node1.addContainer( newEdgeGmlID );
      node2.addContainer( newEdgeGmlID );
      if( useMiddleNode )
        middleNode.addContainer( newEdgeGmlID );
    }
    if( elementLeftID == elementRightID )
    {
      final IElement1D element1D = getElement1D( elementLeftID );
      element1D.setEdge( edge );
    }
    else
    {
      if( elementLeftID != 0 )
      {
        final IPolyElement elementLeft = getElement2D( elementLeftID );
        elementLeft.addEdge( edge.getGmlID() );
        edge.addContainer( elementLeft.getGmlID() );
      }
      if( elementRightID != 0 )
      {
        final IPolyElement elementRight = getElement2D( elementRightID );
        elementRight.addEdge( edge.getGmlID() );
        edge.addContainer( elementRight.getGmlID() );
      }
    }
  }

  private final IFE1D2DNode getNode( final Integer rmaID )
  {
    final String nodeGmlID = m_nodesNameConversionMap.get( rmaID );
    if( nodeGmlID == null )
      return null;
    final Feature nodeFeature = m_workspace.getFeature( nodeGmlID );
    return (IFE1D2DNode) nodeFeature.getAdapter( IFE1D2DNode.class );
  }

  private final IPolyElement getElement2D( final Integer rmaID )
  {
    final String gmlID = m_elementsNameConversionMap.get( rmaID );
    if( gmlID == null )
    {
      final IPolyElement newElement = m_model.getElements().addNew( IPolyElement.QNAME, IPolyElement.class );
      m_createdFeatures.add( newElement );
      m_elementsNameConversionMap.put( rmaID, newElement.getGmlID() );
      return newElement;
    }
    else
      return (IPolyElement) m_workspace.getFeature( gmlID ).getAdapter( IPolyElement.class );
  }

  private final IElement1D getElement1D( final Integer rmaID )
  {
    final String gmlID = m_elementsNameConversionMap.get( rmaID );
    if( gmlID == null )
    {
      final IElement1D newElement = m_model.getElements().addNew( IElement1D.QNAME, IElement1D.class );
      m_createdFeatures.add( newElement );
      m_elementsNameConversionMap.put( rmaID, newElement.getGmlID() );
      return newElement;
    }
    else
      return (IElement1D) m_workspace.getFeature( gmlID ).getAdapter( IElement1D.class );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleElement(java.lang.String, int, int, int,
   *      int)
   */
  public void handleElement( final String lineString, final int id, final int currentRougthnessClassID, final int previousRoughnessClassID, final int eleminationNumber )
  {

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleNode(java.lang.String, int, double, double,
   *      double)
   */
  public void handleNode( final String lineString, final int id, final double xCoord, final double yCoord, final double elevation )
  {
    final GM_Point nodeLocation = m_positionProvider.getGMPoint( xCoord, yCoord, elevation );
    final double x = nodeLocation.getX();
    final double y = nodeLocation.getY();
    IFE1D2DNode existingNode = null;
    for( final GM_Point node : m_pointCashe.keySet() )
    {
      if( node.getX() == x && node.getY() == y )
      {
        existingNode = m_pointCashe.get( node );
        break;
      }
    }

    if( existingNode != null )
    {
      // this means that in .2d file several nodes with different IDs have the same coords!
      // What to do?

      // For the moment, we will assume that it is the same node
      m_nodesNameConversionMap.put( id, existingNode.getGmlID() );
      // Logger.getLogger( DiscretisationModel1d2dHandler.class.getName() ).log( Level.WARNING, "Multiple nodes with the
      // same coordinates found. " + existingNode.getPoint().toString() );
      return;
    }
    final IFE1D2DNode node = m_model.getNodes().addNew( IFE1D2DNode.QNAME, IFE1D2DNode.class );
    node.setPoint( nodeLocation );
    m_pointCashe.put( nodeLocation, node );
    m_createdFeatures.add( node );
    m_nodesNameConversionMap.put( id, node.getGmlID() );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerError(java.lang.String,
   *      org.kalypso.kalypsomodel1d2d.conv.EReadError)
   */
  public void handleError( final String lineString, final EReadError errorHints )
  {
    // FIXE redraw me
    throw new RuntimeException( "bad line=" + lineString ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerUnIdentifyable(java.lang.String)
   */
  public void handlerUnIdentifyable( final String lineString )
  {

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#getCreatedFeatures()
   */
  public List<IFeatureWrapper2> getCreatedFeatures( )
  {
    return m_createdFeatures;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleResult(java.lang.String, int, double,
   *      double, double, double)
   */
  public void handleResult( final String lineString, final int id, final double vx, final double vy, final double depth, final double waterlevel )
  {
    // do nothing, because here just the model is beeing read.

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleTime(java.lang.String, java.util.Date)
   */
  public void handleTime( final String line, final Date time )
  {
    // TODO: maybe set description, ...?
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleJunction(java.lang.String, int, int, int,
   *      int)
   */
  public void handleJunction( final String line, final int junctionID, final int element1dID, final int boundaryLine2dID, final int node1dID )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleFlowResitance(java.lang.String, int,
   *      double, double, double)
   */
  public void handleFlowResitance( String line, int id, double combinedLambda, double soilLambda, double vegetationLambda )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleNodeInformation(java.lang.String, int, int,
   *      double, double, double, double)
   */
  public void handleNodeInformation( String line, int id, int dry, double value1, double value2, double value3, double value4 )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handle1dJunctionInformation(java.lang.String,
   *      int, java.util.List)
   */
  public void handle1dJunctionInformation( String line, int junctionId, List<Integer> junctionNodeIDList )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleTimeDependentAdditionalResult(java.lang.String,
   *      int, double, double, double, org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv.RESULTLINES)
   */
  public void handleTimeDependentAdditionalResult( String lineString, int id, double vx, double vy, double depth, RESULTLINES resultlines )
  {
    // TODO Auto-generated method stub

  }

}
