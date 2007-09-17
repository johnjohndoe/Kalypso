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
import java.util.List;

import org.kalypso.kalypsomodel1d2d.ops.ModelOps;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.EdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IEdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * The handler that converts the rma10s element events into discretisation model elements and links
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

  private final IFeatureWrapperCollection<IFE1D2DNode> m_modelNodes;

  private final IFeatureWrapperCollection<IFE1D2DEdge> m_modelEdges;

  private final IFeatureWrapperCollection<IFE1D2DElement> m_modelElements;

  private final IFeatureWrapperCollection<IFELine> m_modelContiLines;

  private final List<IFeatureWrapper2> m_createdFeatures = new ArrayList<IFeatureWrapper2>();

  private final GMLWorkspace m_workspace;

  /**
   * The provider used for conversion bettween native and gml ids
   */
  private ConversionIDProvider m_modelElementIDProvider;

  private final IPositionProvider m_positionProvider;

  public DiscretisationModel1d2dHandler( final IFEDiscretisationModel1d2d model, final IPositionProvider positionProvider, final ConversionIDProvider modelElementIDProvider )
  {
    // Util.getCommandableWorkspace( modelClass )
    m_model = model;
    m_workspace = model.getWrappedFeature().getWorkspace();
    m_modelNodes = model.getNodes();
    m_modelEdges = model.getEdges();
    m_modelElements = model.getElements();
    m_modelContiLines = model.getContinuityLines();
    m_positionProvider = positionProvider;
    m_modelElementIDProvider = modelElementIDProvider;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#start()
   */
  public void start( )
  {
    System.out.println( "Parse start" );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#end()
   */
  public void end( )
  {
    for( final IFE1D2DElement el : m_modelElements )
    {
      ModelOps.sortElementEdges( el );
    }
    // modelElements.getWrappedFeature().invalidEnvelope();
    m_modelElements.getWrappedList().invalidate();
    m_modelEdges.getWrappedList().invalidate();
    m_modelNodes.getWrappedList().invalidate();
    final int size = m_createdFeatures.size();
    final Feature[] createdFeatures = new Feature[size];
    for( int i = size - 1; i >= 0; i-- )
    {
      createdFeatures[i] = m_createdFeatures.get( i ).getWrappedFeature();
      createdFeatures[i].invalidEnvelope();
    }
    m_workspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_workspace, m_model.getWrappedFeature(), createdFeatures, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
    // workspace.fireModellEvent( new )
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleArc(java.lang.String, int, int, int, int,
   *      int, int)
   */
  public void handleArc( final String lineString, final int id, final int node1ID, final int node2ID, final int elementLeftID, final int elementRightID, final int middleNodeID )
  {
    final String edgeID = m_modelElementIDProvider.rma10sToGmlID( ERma10sModelElementKey.AR, id );
    final String gmlNode1ID = m_modelElementIDProvider.rma10sToGmlID( ERma10sModelElementKey.PE, node1ID );
    final String gmlNode2ID = m_modelElementIDProvider.rma10sToGmlID( ERma10sModelElementKey.PE, node2ID );

    final IFE1D2DNode node1 = getNode( gmlNode1ID );
    final IFE1D2DNode node2 = getNode( gmlNode2ID );
    final Feature edgeFeature = m_workspace.getFeature( edgeID );
    IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge = null;
    if( edgeFeature != null )
      edge = (IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>) edgeFeature.getAdapter( IFE1D2DEdge.class );
    else
    {
      // TODO: this is a major performance bug!
      // searching AND adding many new nodes causes the SplitSort of the edge-list to resort for every new egde

      // TODO: commented it otu for the moment, because never an edge was found for
      // existing valid .2d files
      // check what to do
      edge = m_model.findEdge( node1, node2 );
      if( edge != null )
      {
        System.out.println( Messages.getString( "DiscretisationModel1d2dHandler.1" ) ); //$NON-NLS-1$
      }
    }

    // boolean isNew=false;
    if( edge == null )
    {
      edge = m_modelEdges.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE, edgeID );
      m_createdFeatures.add( edge );
      edge.addNode( gmlNode1ID );
      edge.addNode( gmlNode2ID );
      node1.addContainer( edgeID );
      node2.addContainer( edgeID );
      // isNew=true;
    }
    // else
    // // this never happens for existing (valid) .2d files
    // // se coomend on find edges above
    // System.out.println( "Edge already there: " + edge );

    // TODO set elements
    if( elementLeftID == elementRightID )
    {
      final String gmlID = m_modelElementIDProvider.rma10sToGmlID( ERma10sModelElementKey.FE, elementLeftID );
      final IElement1D ele1D = getElement1D( gmlID );
      ele1D.setEdge( edge );
      // one d element
      // System.out.println("1D elements not supported!");
      // throw new RuntimeException("2d ONLY for now");
    }
    else
    {
      if( elementLeftID != 0 )
      {
        try
        {
          final String gmlID = m_modelElementIDProvider.rma10sToGmlID( ERma10sModelElementKey.FE, elementLeftID );
          final IPolyElement eleLeft = getElement2D( gmlID );
          eleLeft.addEdge( edgeID );
          edge.addContainer( gmlID );
        }
        catch( final Throwable th )
        {
          th.printStackTrace();
        }
      }

      if( elementRightID != 0 )
      {
        try
        {
          final String gmlID = m_modelElementIDProvider.rma10sToGmlID( ERma10sModelElementKey.FE, elementRightID );
          final IPolyElement eleRight = getElement2D( gmlID );
          // TODO remove dependencies to the inv edge use find node instead
          // change the api to get the whether is was newly created or not
          IEdgeInv inv = edge.getEdgeInv();
          if( inv == null )
          {
            inv = new EdgeInv( edge, m_model );
            m_createdFeatures.add( inv );
          }
          eleRight.addEdge( inv.getGmlID() );
          inv.addContainer( gmlID );
        }
        catch( final Throwable th )
        {
          th.printStackTrace();
        }
      }
    }
    // TODO add middle node

  }

  private final IFE1D2DNode getNode( final String gmlID )
  {
    final Feature nodeFeature = m_workspace.getFeature( gmlID );
    final IFE1D2DNode node = (IFE1D2DNode) nodeFeature.getAdapter( IFE1D2DNode.class );
    return node;
  }

  private final IPolyElement getElement2D( final String gmlID )
  {
    final Feature eleFeature = m_workspace.getFeature( gmlID );
    if( eleFeature == null )
    {
      final IPolyElement addNew = m_modelElements.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT, gmlID, IPolyElement.class );
      m_createdFeatures.add( addNew );
      return addNew;
    }
    else
    {
      return (IPolyElement) eleFeature.getAdapter( IPolyElement.class );
    }
  }

  private final IElement1D getElement1D( final String gmlID )
  {
    final Feature eleFeature = m_workspace.getFeature( gmlID );
    if( eleFeature == null )
    {
      final IElement1D addNew = m_modelElements.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT, gmlID, IElement1D.class );
      m_createdFeatures.add( addNew );
      return addNew;
    }
    else
    {
      return (IElement1D) eleFeature.getAdapter( IElement1D.class );
    }
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
  public void handleNode( final String lineString, final int id, final double easting, final double northing, final double elevation )
  {
    // TODO use model.createNode to create from position
    final String gmlID = m_modelElementIDProvider.rma10sToGmlID( ERma10sModelElementKey.PE, id );
    if( m_workspace.getFeature( gmlID ) != null )
    {
      return;
    }

    final IFE1D2DNode node = m_modelNodes.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_NODE, gmlID );
    m_createdFeatures.add( node );
    final GM_Point newLocation = m_positionProvider.getGMPoint( easting,// nativeX,
    northing/* nativeY */, elevation// nativeZ
    );
    node.setPoint( newLocation );

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerError(java.lang.String,
   *      org.kalypso.kalypsomodel1d2d.conv.EReadError)
   */
  public void handleError( final String lineString, final EReadError errorHints )
  {
    // FIXE redaw me
    throw new RuntimeException( "bad line=" + lineString ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerUnIdentifyable(java.lang.String)
   */
  public void handlerUnIdentifyable( final String lineString )
  {

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#setIRoughnessIDProvider(org.kalypso.kalypsomodel1d2d.conv.IRoughnessIDProvider)
   */
  public void setIRoughnessIDProvider( final IRoughnessIDProvider roughnessIDProvider ) throws IllegalArgumentException
  {
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#setModelElementIDProvider(org.kalypso.kalypsomodel1d2d.conv.IModelElementIDProvider)
   */
  public void setModelElementIDProvider( final ConversionIDProvider modelElementIDProvider ) throws IllegalArgumentException
  {
    this.m_modelElementIDProvider = modelElementIDProvider;
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

}
