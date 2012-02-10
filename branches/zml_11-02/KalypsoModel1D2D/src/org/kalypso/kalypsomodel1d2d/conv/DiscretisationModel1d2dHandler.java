/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv.RESULTLINES;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ops.ModelOps;
import org.kalypso.kalypsomodel1d2d.ops.TypeInfo;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteCmdFactory;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeletePolyElementCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;

/**
 * The handler that converts the RMA-Kalypso element events into discretisation model elements and links
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 * @author Patrice Congo
 * 
 */
@SuppressWarnings("unchecked")
public class DiscretisationModel1d2dHandler implements IRMA10SModelElementHandler
{
  /**
   * The model to fill with the parsed fe element from
   */
  private final IFEDiscretisationModel1d2d m_model;

  private final GMLWorkspace m_workspace;

  private final IPositionProvider m_positionProvider;

  private final HashMap<Integer, String> m_nodesNameConversionMap = new HashMap<Integer, String>();

  private final HashMap<Integer, String> m_edgesNameConversionMap = new HashMap<Integer, String>();

  private final HashMap<Integer, String> m_elementsNameConversionMap = new HashMap<Integer, String>();

  private GM_Envelope m_gmExistingEnvelope;

  private Set<Integer> m_setNotInsertedNodes;

  private static boolean[] NOT_CREATED = new boolean[1];

  public DiscretisationModel1d2dHandler( final IFEDiscretisationModel1d2d model, final IPositionProvider positionProvider )
  {
    m_model = model;
    m_workspace = model.getFeature().getWorkspace();
    m_positionProvider = positionProvider;
    m_setNotInsertedNodes = new HashSet<Integer>();
    try
    {
      m_gmExistingEnvelope = m_model.getNodes().getBoundingBox();
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#start()
   */
  @Override
  public void start( )
  {
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#end()
   */
  @Override
  public void end( )
  {
    final Feature[] lElementsToRemove = getElementsWithoutGeometry();
    final Feature[] lAllElements = m_model.getElements().getWrappedList().toFeatures();

    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_model.getFeature(), lAllElements, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

    removeElements( lElementsToRemove );
  }

  private void removeElements( Feature[] elementsToRemove )
  {
    final IDiscrModel1d2dChangeCommand deleteCmdPolyElement = DeleteCmdFactory.createDeleteCmdPoly( m_model );

    for( final Feature feature : elementsToRemove )
    {
      if( feature != null )
      {

        if( TypeInfo.isPolyElementFeature( feature ) )
        {
          ((DeletePolyElementCmd) deleteCmdPolyElement).addElementToRemove( feature );
        }
      }
    }
    try
    {
      deleteCmdPolyElement.process();
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    m_model.getElements().removeAllAtOnce( Arrays.asList( elementsToRemove ) );
    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_model.getFeature(), elementsToRemove, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE ) );
  }

  private Feature[] getElementsWithoutGeometry( )
  {
    Set<Feature> lSetToRemove = new HashSet<Feature>();
    for( final IFE1D2DElement lElement : m_model.getElements() )
    {
      if( lElement instanceof IPolyElement )
      {
        final GM_Surface<GM_SurfacePatch> eleGeom = ((IPolyElement) lElement).getGeometry();
        if( eleGeom == null )
        {
          lSetToRemove.add( lElement.getFeature() );
        }
      }
    }
    return lSetToRemove.toArray( new Feature[lSetToRemove.size()] );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleArc(java.lang.String, int, int, int, int,
   *      int, int)
   */
  @Override
  public void handleArc( final String lineString, final int id, final int node1ID, final int node2ID, final int elementLeftID, final int elementRightID, final int middleNodeID )
  {
    final IFE1D2DNode node1 = getNode( node1ID );
    final IFE1D2DNode node2 = getNode( node2ID );
    if( node1 == null )
    {
      if( !m_setNotInsertedNodes.contains( node1ID ) )
      {
        throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.DiscretisationModel1d2dHandler.0", node1ID, id ) ); //$NON-NLS-1$
      }
      return;
    }
    if( node2 == null )
    {
      if( !m_setNotInsertedNodes.contains( node2ID ) )
      {
        throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.DiscretisationModel1d2dHandler.1", node1ID, id ) ); //$NON-NLS-1$
      }
      return;
    }

    final IFE1D2DEdge existingEdge = m_model.findEdge( node1, node2 );
    final IFE1D2DEdge edge;
    if( existingEdge != null )
    {
      edge = existingEdge;
    }
    else
    {
      edge = FE1D2DEdge.createFromModel( m_model, node1, node2 );
    }
    final String gmlID = edge.getGmlID();
    m_edgesNameConversionMap.put( id, gmlID );
    if( elementLeftID == elementRightID )
    {
      // 1d
      maybeAddNewElement1d( elementLeftID, edge );
    }
    else
    {
      // 2d
      maybeAddEdgeToElement( elementLeftID, edge );
      maybeAddEdgeToElement( elementRightID, edge );
    }
  }

  private final IFE1D2DNode getNode( final int rmaID )
  {
    final String nodeGmlID = m_nodesNameConversionMap.get( rmaID );
    if( nodeGmlID == null )
      return null;
    final Feature nodeFeature = m_workspace.getFeature( nodeGmlID );
    return (IFE1D2DNode) nodeFeature.getAdapter( IFE1D2DNode.class );
  }

  private final void maybeAddEdgeToElement( final int rmaID, final IFE1D2DEdge edge )
  {
    final String edgeId = edge.getGmlID();

    IFeatureWrapperCollection lContainers = edge.getContainers();
    int iCountPolyElements = 0;
    for( int i = 0; i < lContainers.size(); ++i )
    {
      Object lFeature = lContainers.get( i );
      if( lFeature instanceof IPolyElement )
      {
        iCountPolyElements++;
        if( rmaID != 0 )
        {
          final String gmlID = m_elementsNameConversionMap.get( rmaID );

          if( gmlID == null && !m_elementsNameConversionMap.values().contains( ((IPolyElement) lFeature).getGmlID() ) )
          {
            m_elementsNameConversionMap.put( rmaID, ((IPolyElement) lFeature).getGmlID() );
          }
        }
      }
      if( iCountPolyElements == 2 )
      {
        return;
      }
    }
    final IPolyElement element;
    if( rmaID == 0 )
    {
      // this is either the outer boundary or an adjacent existing element
      // try to find an element in the model that lies on the edge
      final GM_Point middleNodePoint = edge.getMiddleNodePoint();
      final IPolyElement existingElement2d = m_model.find2DElement( middleNodePoint, 0.01 );
      if( existingElement2d != null )
      {
        element = existingElement2d;
      }
      else
      {
        element = null;
      }
    }
    else
    {
      final String gmlID = m_elementsNameConversionMap.get( rmaID );
      if( gmlID == null )
      {
        // this is a new element
        element = m_model.getElements().addNew( IPolyElement.QNAME, IPolyElement.class );
      }
      else
      {
        // this is an imported element
        element = (IPolyElement) m_workspace.getFeature( gmlID ).getAdapter( IPolyElement.class );
      }
    }
    if( element != null )
    {
      // add edge to element and element to edge
      final String elementId = element.getGmlID();
      element.addEdge( edgeId );
      edge.addContainer( elementId );
      if( rmaID != 0 )
      {
        // remember imported element
        m_elementsNameConversionMap.put( rmaID, elementId );
      }
    }
  }

  private final void maybeAddNewElement1d( final int rmaID, final IFE1D2DEdge edge )
  {
    final String gmlID = m_elementsNameConversionMap.get( rmaID );
    if( gmlID == null )
    {
      final IElement1D existingElement1d = m_model.find1DElement( edge.getMiddleNodePoint(), 0.01 );
      final IElement1D element1d;
      if( existingElement1d != null )
      {
        element1d = existingElement1d;
      }
      else
      {
        element1d = ModelOps.createElement1d( m_model, edge );
      }
      m_elementsNameConversionMap.put( rmaID, element1d.getGmlID() );
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleElement(java.lang.String, int, int, int,
   *      int)
   */
  @Override
  public void handleElement( final String lineString, final int id, final int currentRougthnessClassID, final int previousRoughnessClassID, final int eleminationNumber )
  {

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleNode(java.lang.String, int, double, double,
   *      double)
   */
  @Override
  public void handleNode( final String lineString, final int id, final double xCoord, final double yCoord, final double elevation )
  {
    IFE1D2DNode node = getNode( id );
    if( node != null )
    {
      // this means that in .2d file several nodes with different IDs have the same coords!
      // What to do?
      // For the moment, we will assume that it is the same node
      Logger.getLogger( DiscretisationModel1d2dHandler.class.getName() ).log( Level.WARNING, Messages.getString( "org.kalypso.kalypsomodel1d2d.conv.DiscretisationModel1d2dHandler.3", node.getPoint().toString() ) ); //$NON-NLS-1$
      return;
    }

    final GM_Point nodeLocation = m_positionProvider.getGMPoint( xCoord, yCoord, elevation );
    node = m_model.findNode( nodeLocation, 0.01 );
    if( node == null )
    {
      if( m_gmExistingEnvelope != null && m_gmExistingEnvelope.contains( nodeLocation.getPosition() ) )
      {
        IPolyElement lFoundElement = m_model.find2DElement( nodeLocation, 0.01 );
        if( lFoundElement != null )
        {
          // do not insert nodes that are placed on existing model(overlapped elements)
          m_setNotInsertedNodes.add( id );
          Logger.getLogger( DiscretisationModel1d2dHandler.class.getName() ).log( Level.WARNING, "removed node ", nodeLocation.toString() ); //$NON-NLS-1$
          return;
        }
      }
      // new node, create
      node = m_model.createNode( nodeLocation, -1, NOT_CREATED );
    }
    m_nodesNameConversionMap.put( id, node.getGmlID() );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerError(java.lang.String,
   *      org.kalypso.kalypsomodel1d2d.conv.EReadError)
   */
  @Override
  public void handleError( final String lineString, final EReadError errorHints )
  {
    // FIXE redraw me
    throw new RuntimeException( "bad line=" + lineString ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerUnIdentifyable(java.lang.String)
   */
  @Override
  public void handlerUnIdentifyable( final String lineString )
  {

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleResult(java.lang.String, int, double,
   *      double, double, double)
   */
  @Override
  public void handleResult( final String lineString, final int id, final double vx, final double vy, final double depth, final double waterlevel )
  {
    // do nothing, because here just the model is beeing read.

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleTime(java.lang.String, java.util.Date)
   */
  @Override
  public void handleTime( final String line, final Date time )
  {
    // TODO: maybe set description, ...?
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleJunction(java.lang.String, int, int, int,
   *      int)
   */
  @Override
  public void handleJunction( final String line, final int junctionID, final int element1dID, final int boundaryLine2dID, final int node1dID )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleFlowResitance(java.lang.String, int,
   *      double, double, double)
   */
  @Override
  public void handleFlowResitance( String line, int id, double combinedLambda, double soilLambda, double vegetationLambda )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleNodeInformation(java.lang.String, int, int,
   *      double, double, double, double)
   */
  @Override
  public void handleNodeInformation( String line, int id, int dry, double value1, double value2, double value3, double value4 )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handle1dJunctionInformation(java.lang.String,
   *      int, java.util.List)
   */
  @Override
  public void handle1dJunctionInformation( String line, int junctionId, List<Integer> junctionNodeIDList )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleTimeDependentAdditionalResult(java.lang.String,
   *      int, double, double, double, org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv.RESULTLINES)
   */
  @Override
  public void handleTimeDependentAdditionalResult( String lineString, int id, double vx, double vy, double depth, RESULTLINES resultlines )
  {
    // TODO Auto-generated method stub

  }

}
