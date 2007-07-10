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

import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.ops.ModelOps;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.EdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IEdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * An handler that convert the discretisation model element and link events into rma10s elements
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 * 
 */
@SuppressWarnings("unchecked")
public class RMA10sModelHandler implements IRMA10SModelElementHandler
{
  /**
   * The model to fill with the parsed fe element from
   */
  private final IFEDiscretisationModel1d2d model;

  private final IFeatureWrapperCollection<IFE1D2DNode> modelNodes;

  private final IFeatureWrapperCollection<IFE1D2DEdge> modelEdges;

  private final IFeatureWrapperCollection<IFE1D2DElement> modelElements;

  // private CS_CoordinateSystem coordinateSystem;
  private final GMLWorkspace workspace;

  /**
   * The provider used for convertion of native roughness ids to gml model roughness id.
   */
  private IRoughnessIDProvider roughnessIDProvider;

  /**
   * The provider used for conversion bettween native and gml ids
   */
  private IModelElementIDProvider modelElementIDProvider;

  private final IPositionProvider positionProvider;

  /**
   * 
   */
  public RMA10sModelHandler( final IFEDiscretisationModel1d2d model, final IPositionProvider positionProvider, final IModelElementIDProvider modelElementIDProvider )
  {
    this.model = model;
    workspace = model.getWrappedFeature().getWorkspace();
    modelNodes = model.getNodes();
    modelEdges = model.getEdges();
    modelElements = model.getElements();
    this.positionProvider = positionProvider;
    this.modelElementIDProvider = modelElementIDProvider;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#end()
   */
  public void end( )
  {
    for( final IFE1D2DElement el : modelElements )
    {
      ModelOps.sortElementEdges( el );
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleArc(java.lang.String, int, int, int, int,
   *      int, int)
   */
  public void handleArc( final String lineString, final int id, final int node1ID, final int node2ID, final int elementLeftID, final int elementRightID, final int middleNodeID )
  {

    final String edgeID = modelElementIDProvider.rma10sToGmlID( ERma10sModelElementKey.AR, id );
    final String gmlNode1ID = modelElementIDProvider.rma10sToGmlID( ERma10sModelElementKey.PE, node1ID );
    final String gmlNode2ID = modelElementIDProvider.rma10sToGmlID( ERma10sModelElementKey.PE, node2ID );

    final IFE1D2DNode<IFE1D2DEdge> node1 = getNode( gmlNode1ID );
    final IFE1D2DNode<IFE1D2DEdge> node2 = getNode( gmlNode2ID );
    final Feature edgeFeature = workspace.getFeature( edgeID );
    IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge = null;
    if( edgeFeature != null )
    {
      edge = (IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>) edgeFeature.getAdapter( IFE1D2DEdge.class );
    }
    else
    {
      edge = model.findEdge( node1, node2 );
    }

    if( edge == null )
    {
      edge = modelEdges.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE, edgeID );
      edge.addNode( gmlNode1ID );
      edge.addNode( gmlNode2ID );
    }
    // TODO set elements
    if( elementLeftID == elementRightID )
    {
      // one d element
      throw new RuntimeException( "2d ONLY for now" );
    }
    else
    {
      if( elementLeftID != 0 )
      {
        try
        {
          final String gmlID = modelElementIDProvider.rma10sToGmlID( ERma10sModelElementKey.FE, elementLeftID );
          final IPolyElement eleLeft = getElement2D( gmlID );
          final int size = eleLeft.getEdges().size();
          eleLeft.addEdge( edgeID );
          if( eleLeft.getEdges().size() - size != 1 )
          {
            System.out.println( "BAd Increment=" + gmlID + " AR" + edgeID );
          }

          edge.addContainer( gmlID );
        }
        catch( final Throwable th )
        {
          th.printStackTrace();
        }
      }
      else
      {
        System.out.println( "BorderEdge=" + edgeID );
      }

      if( elementRightID != 0 )
      {
        try
        {
          final String gmlID = modelElementIDProvider.rma10sToGmlID( ERma10sModelElementKey.FE, elementRightID );
          final IPolyElement eleRight = getElement2D( gmlID );
          // TODO remove dependencies to the inv edge use find node instead
          // change the api to get the whether is was newly created or not
          final IEdgeInv inv = new EdgeInv( edge, model );
          final int size = eleRight.getEdges().size();
          eleRight.addEdge( inv.getGmlID() );
          if( eleRight.getEdges().size() - size != 1 )
          {
            System.out.println( "BAd Increment=" + gmlID + " AR" + inv.getGmlID() );
          }
          inv.addContainer( gmlID );
        }
        catch( final Throwable th )
        {
          th.printStackTrace();
        }
      }
      else
      {
        System.out.println( "BorderEdge=" + edgeID );
      }
    }
    // TODO add middle node

  }

  private final IFE1D2DNode<IFE1D2DEdge> getNode( final String gmlID )
  {
    final Feature nodeFeature = workspace.getFeature( gmlID );

    final IFE1D2DNode<IFE1D2DEdge> node = (IFE1D2DNode<IFE1D2DEdge>) nodeFeature.getAdapter( IFE1D2DNode.class );
    return node;
  }

  private final IPolyElement getElement2D( final String gmlID )
  {
    final Feature eleFeature = workspace.getFeature( gmlID );
    if( eleFeature == null )
    {
      return modelElements.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT, gmlID, IPolyElement.class );
    }
    else
    {
      return (IPolyElement) eleFeature.getAdapter( IFE1D2DElement.class );
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
    final String gmlID = modelElementIDProvider.rma10sToGmlID( ERma10sModelElementKey.PE, id );
    if( workspace.getFeature( gmlID ) != null )
    {
      return;
    }

    final IFE1D2DNode<IFE1D2DEdge> node = modelNodes.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_NODE, gmlID );
    // node.setPoint(
    // GeometryFactory.createGM_Point(
    // easting+35*100000,
    // northing+35*100000,
    // elevation,
    // coordinateSystem ));
    final GM_Point newLocation = positionProvider.getGMPoint( easting,// nativeX,
    northing/* nativeY */, elevation// nativeZ
    );
    node.setPoint( newLocation );

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerError(java.lang.String,
   *      org.kalypso.kalypsomodel1d2d.conv.EReadError)
   */
  public void handlerError( final String lineString, final EReadError errorHints )
  {
    // FIXE redaw me
    // throw new RuntimeException("bad line="+lineString);
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerUnIdentifyable(java.lang.String)
   */
  public void handlerUnIdentifyable( final String lineString )
  {

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#start()
   */
  public void start( )
  {
    System.out.println( "Parse start" );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#setIRoughnessIDProvider(org.kalypso.kalypsomodel1d2d.conv.IRoughnessIDProvider)
   */
  public void setIRoughnessIDProvider( final IRoughnessIDProvider roughnessIDProvider ) throws IllegalArgumentException
  {
    this.roughnessIDProvider = roughnessIDProvider;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#setModelElementIDProvider(org.kalypso.kalypsomodel1d2d.conv.IModelElementIDProvider)
   */
  public void setModelElementIDProvider( final IModelElementIDProvider modelElementIDProvider ) throws IllegalArgumentException
  {
    this.modelElementIDProvider = modelElementIDProvider;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#getCreatedFeatures()
   */
  public List<IFeatureWrapper2> getCreatedFeatures( )
  {
    return Collections.emptyList();
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
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleJunction(java.lang.String, int, int, int,
   *      int)
   */
  public void handleJunction( final String line, final int junctionID, final int element1dID, final int boundaryLine2dID, final int node1dID )
  {
    // TODO Auto-generated method stub

  }

}
