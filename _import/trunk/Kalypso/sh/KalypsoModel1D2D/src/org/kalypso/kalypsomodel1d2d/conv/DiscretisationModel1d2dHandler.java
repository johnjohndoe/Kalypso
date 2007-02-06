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

import org.kalypso.kalypsomodel1d2d.ops.ModelOps;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.EdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.IEdgeInv;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * An handler that convert the rma10s element events into
 * discretisation model elements and links
 * 
 * @author Patrice Congo
 *
 */
@SuppressWarnings("unchecked")
public class DiscretisationModel1d2dHandler implements IRMA10SModelElementHandler
{
  /**
   * The model to fill with the parsed fe element from 
   */
  private IFEDiscretisationModel1d2d model;
  
  private IFeatureWrapperCollection<IFE1D2DNode> modelNodes;
  private IFeatureWrapperCollection<IFE1D2DEdge> modelEdges;
  private IFeatureWrapperCollection<IFE1D2DElement> modelElements;
  private CS_CoordinateSystem coordinateSystem;
  private GMLWorkspace workspace;
  
  /**
   * The provider used for convertion of native roughness
   * ids to gml model roughness id. 
   */
  private IRoughnessIDProvider roughnessIDProvider;
  
  /**
   * The provider used for conversion bettween native and gml
   * ids
   */
  private IModelElementIDProvider modelElementIDProvider;
  
  /**
   * 
   */
  public DiscretisationModel1d2dHandler(
                             IFEDiscretisationModel1d2d model,
                             CS_CoordinateSystem coordinateSystem,
                             IModelElementIDProvider modelElementIDProvider)
  {
    this.model=model;
    workspace=model.getWrappedFeature().getWorkspace();
    modelNodes=model.getNodes();
    modelEdges=model.getEdges();
    modelElements=model.getElements();
    this.coordinateSystem=coordinateSystem;
    this.modelElementIDProvider=modelElementIDProvider;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#end()
   */
  public void end( )
  {
    for(IFE1D2DElement el:modelElements)
    {
      ModelOps.sortElementEdges( el );      
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleArc(java.lang.String, int, int, int, int, int, int)
   */
  public void handleArc( 
                  String lineString, 
                  int id, 
                  int node1ID, 
                  int node2ID, 
                  int elementLeftID, 
                  int elementRightID, 
                  int middleNodeID )
  {
    
    String edgeID=
      modelElementIDProvider.rma10sToGmlID( ERma10sModelElementKey.AR, id);
    String gmlNode1ID=
      modelElementIDProvider.rma10sToGmlID( 
                    ERma10sModelElementKey.PE, node1ID);
    String gmlNode2ID=
      modelElementIDProvider.rma10sToGmlID( 
                    ERma10sModelElementKey.PE, node2ID);
    
    IFE1D2DNode<IFE1D2DEdge> node1=getNode( gmlNode1ID );
    IFE1D2DNode<IFE1D2DEdge> node2=getNode( gmlNode2ID );
    Feature edgeFeature=workspace.getFeature( edgeID );
    IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge = null;
    if(edgeFeature!=null)
    {
      edge=(IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>)
              edgeFeature.getAdapter( IFE1D2DEdge.class );
    }
    else
    {
       edge=model.findEdge( node1, node2 );
    }
    
    boolean isNew=false;
    if(edge==null)
    {
      edge=
        modelEdges.addNew( 
          Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE, 
          edgeID );
      edge.addNode( gmlNode1ID );
      edge.addNode( gmlNode2ID);
      isNew=true;
    }
    //TODO set elements
    if(elementLeftID==elementRightID)
    {
      //one d element
      throw new RuntimeException("2d ONLY for now");
    }
    else
    {
      if(elementLeftID!=0)
      {
        try
        {
          String gmlID=
            modelElementIDProvider.rma10sToGmlID( 
                                  ERma10sModelElementKey.FE, 
                                  elementLeftID);
          IFE1D2DElement eleLeft=getElement2D(gmlID);
          int size=eleLeft.getEdges().size();
          eleLeft.addEdge( edgeID );
          if(eleLeft.getEdges().size()-size!=1)
          {
            System.out.println("BAd Increment="+gmlID+" AR"+edgeID);
          }
          
          edge.addContainer( gmlID );
        }
        catch(Throwable th)
        {
          th.printStackTrace();
        }
      }
      else
      {
        System.out.println("BorderEdge="+edgeID);
      }
      
      if(elementRightID!=0)
      {
        try
        {
          String gmlID=
            modelElementIDProvider.rma10sToGmlID( 
                        ERma10sModelElementKey.FE, elementRightID);
          IFE1D2DElement eleRight=getElement2D(gmlID);
          IEdgeInv inv= new EdgeInv(edge,model);
          int size=eleRight.getEdges().size();
          eleRight.addEdge( inv.getGmlID() );
          if(eleRight.getEdges().size()-size!=1)
          {
            System.out.println("BAd Increment="+gmlID+" AR"+inv.getGmlID());
          }
          inv.addContainer( gmlID );
        }
        catch( Throwable th )
        {
          th.printStackTrace();
        }
      } 
      else
      {
        System.out.println("BorderEdge="+edgeID);
      }
    }
    //TODO add middle node
    
  }

  private final IFE1D2DNode<IFE1D2DEdge> getNode(String gmlID)
  {
    Feature nodeFeature=
      workspace.getFeature(gmlID);
    
    IFE1D2DNode<IFE1D2DEdge> node=
        (IFE1D2DNode<IFE1D2DEdge>)nodeFeature.getAdapter(IFE1D2DNode.class);
    return node;
  }
  
  
  private final IFE1D2DElement getElement2D(String gmlID)
  {
    Feature eleFeature=workspace.getFeature( gmlID );
    if(eleFeature==null)
    {
     return modelElements.addNew( 
          Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT,
          gmlID);
    }
    else
    {
      return (IFE1D2DElement)eleFeature.getAdapter( IFE1D2DElement.class );
    }
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleElement(java.lang.String, int, int, int, int)
   */
  public void handleElement( 
                    String lineString, 
                    int id, 
                    int currentRougthnessClassID, 
                    int previousRoughnessClassID, 
                    int eleminationNumber )
  {
    
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleNode(java.lang.String, int, double, double, double)
   */
  public void handleNode( 
                String lineString, 
                int id, 
                double easting, 
                double northing, 
                double elevation )
  {
       String gmlID=
           modelElementIDProvider.rma10sToGmlID( ERma10sModelElementKey.PE, id );
       if(workspace.getFeature( gmlID ) !=null)
       {
           return ;
       }
       
       IFE1D2DNode<IFE1D2DEdge> node= 
                     modelNodes.addNew( 
                           Kalypso1D2DSchemaConstants.WB1D2D_F_NODE,
                           gmlID );
       node.setPoint( 
           GeometryFactory.createGM_Point(
                             easting+35*100000,
                             northing+35*100000,
                             elevation,
                             coordinateSystem ));
       
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerError(java.lang.String, org.kalypso.kalypsomodel1d2d.conv.EReadError)
   */
  public void handlerError( String lineString, EReadError errorHints )
  {
    //FIXE redaw me
    //throw new RuntimeException("bad line="+lineString);
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerUnIdentifyable(java.lang.String)
   */
  public void handlerUnIdentifyable( String lineString )
  {
    
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#start()
   */
  public void start( )
  {
    System.out.println("Parse start");
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#setIRoughnessIDProvider(org.kalypso.kalypsomodel1d2d.conv.IRoughnessIDProvider)
   */
  public void setIRoughnessIDProvider( 
                        IRoughnessIDProvider roughnessIDProvider ) 
                        throws IllegalArgumentException
  {
    this.roughnessIDProvider=roughnessIDProvider;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#setModelElementIDProvider(org.kalypso.kalypsomodel1d2d.conv.IModelElementIDProvider)
   */
  public void setModelElementIDProvider( 
                    IModelElementIDProvider modelElementIDProvider ) 
                    throws IllegalArgumentException
  {
    this.modelElementIDProvider=modelElementIDProvider;
  }
  
  

}
