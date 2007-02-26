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
package org.kalypso.kalypsomodel1d2d.schema.binding;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;


/**
 * Provide a implementation of {@link IFEDiscretisationModel1d2d} to
 * bind wb1d2d:Discretisation gml elements
 * 
 * @author Gernot Belger
 * @author Patrice Congo
 */
@SuppressWarnings("unchecked")
public class FE1D2DDiscretisationModel 
                            extends AbstractFeatureBinder 
                            implements IFEDiscretisationModel1d2d
{
  
  private IFeatureWrapperCollection<IFE1D2DElement> m_elements = 
            new FeatureWrapperCollection<IFE1D2DElement>( 
                    getFeature(), 
                    IFE1D2DElement.class, 
                    Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENTS );

  private IFeatureWrapperCollection<IFE1D2DEdge> m_edges = 
            new FeatureWrapperCollection<IFE1D2DEdge>( 
                  getFeature(), 
                  IFE1D2DEdge.class, 
                  Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGES );

  private IFeatureWrapperCollection<IFE1D2DNode> m_nodes = 
            new FeatureWrapperCollection<IFE1D2DNode>( 
                      getFeature(), 
                      IFE1D2DNode.class, 
                      Kalypso1D2DSchemaConstants.WB1D2D_PROP_NODES
                      );
  
  private IFeatureWrapperCollection<IFE1D2DComplexElement> complexElements=
          new FeatureWrapperCollection<IFE1D2DComplexElement>( 
              getFeature(), 
              IFE1D2DComplexElement.class, 
              Kalypso1D2DSchemaConstants.WB1D2D_PROP_COMPLEX_ELEMENTS
              );
  
  

  public FE1D2DDiscretisationModel( final Feature featureToBind )
  {
    super( 
        featureToBind, 
        Kalypso1D2DSchemaConstants.WB1D2D_F_DiscretisationModel);
    
//    featureToBind.getWorkspace().addModellListener( meListener );
//    System.out.println("Model event added");
    
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d#findEdge(org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode, org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode)
   */
  public IFE1D2DEdge findEdge( final IFE1D2DNode node0, final IFE1D2DNode node1 )
  {
//    final List edgeList = 
//        (List) getFeature().getProperty( 
//            Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGES );
//    // TODO: brute force search, check if this scales good with big models
//    for( final Object object : edgeList )
//    {
//      final FE1D2DEdge edge = new FE1D2DEdge( (Feature) object );
//      final FE1D2DNode[] nodes = edge.getNodesAsArray();
//
//      if( nodes.length != 2 )
//        return null;
//
//      if( (node0.equals( nodes[0] ) && node1.equals( nodes[1] )) || (node0.equals( nodes[1] ) && node1.equals( nodes[0] )) )
//        return edge;
//    }
//
//    return null;
      List<IFE1D2DEdge> edges= new ArrayList<IFE1D2DEdge>();
      edges.addAll(node0.getContainers() );
      if(edges.size()==0)
      {
        return null;
      }
      edges.retainAll( node1.getContainers() );
      int size=edges.size();
      if(size==1)
      {
        IFE1D2DEdge edge=edges.get( 0 );
        if(edge.equals( node0 ))
        {
          return edges.get( 0 );
        }
        else
        {
          IEdgeInv edgeInv=edge.getEdgeInv();
          if(edgeInv==null)
          {
            edgeInv=new EdgeInv(edge,this);
          }
          return edgeInv;
          
        }
      }
      else if(size==2)
      {
        //found edge and edgeinv
        IFE1D2DEdge edge=edges.get( 0 );
        if(edge.getNode( 0 ).getGmlID().equals( node0.getGmlID() ))
        {
          return edge;
        }
        else
        {
          return edges.get( 1 );
        }
      }
      else if(size==0)
      {
        return null;
      }
      else
      {
         System.out.println(
             "Found an several edges with those two nodes:"+edges);
         return null;
      }
      
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d#getComplexElements()
   */
  public IFeatureWrapperCollection<IFE1D2DComplexElement> getComplexElements( )
  {
    return complexElements;
  }
  public final IFeatureWrapperCollection<IFE1D2DElement> getElements( )
  {
    return m_elements;
  }

  public IFeatureWrapperCollection<IFE1D2DNode> getNodes( )
  {
    return m_nodes;
  }

  public IFeatureWrapperCollection<IFE1D2DEdge> getEdges( )
  {
    return m_edges;
  }

  public IFE1D2DContinuityLine<IFE1D2DComplexElement, IFE1D2DEdge> createContinuityLine( )
  {
    final Feature parentFeature = getFeature();
    final IFeatureType parentFT = parentFeature.getFeatureType();
    final IRelationType rt = 
        (IRelationType) parentFT.getProperty( 
              Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENTS );

    final IFeatureType contiType = 
          parentFT.getGMLSchema().getFeatureType( 
              Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DContinuityLine );
    final Feature contiFeature = 
              parentFeature.getWorkspace().createFeature( 
                                    parentFeature, rt, contiType );
    return new FE1D2DContinuityLine( contiFeature );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper#getWrappedFeature()
   */
  public Feature getWrappedFeature( )
  {
    return m_featureToBind;
  }
  
  /**
   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper#getGmlID()
   */
  public String getGmlID( )
  {
    return m_featureToBind.getId();
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d#createNode(GM_Point, double, boolean[])
   */
  public IFE1D2DNode createNode( 
                          GM_Point nodeLocation,
                          double searchRectWidth,
                          boolean[] alreadyExists)
  { 
    Assert.throwIAEOnNullParam( nodeLocation, "nodeLocation" );
    
    IFE1D2DNode node=null;
    if(searchRectWidth>=0)
    {
      //donot search if rect width is negative
      node=findNode( nodeLocation, searchRectWidth );
    }
    if(node!=null)
    {
      if(alreadyExists!=null)
      {
        if(alreadyExists.length>0)
        {
          alreadyExists[0]=false;
        }
      }
      return node;
    }
    else
    {
//      FeatureList nodeList = m_nodes.getWrappedList();
      
      node =  m_nodes.addNew( Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
      node.setPoint( nodeLocation );
      alreadyExists[0]=false;
      return node;      
    }
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d#findNode(org.kalypsodeegree.model.geometry.GM_Point, double)
   */
  public IFE1D2DNode findNode( GM_Point nodeLocation, double searchRectWidth )
  {
    List<Feature> foundNodes= new ArrayList<Feature>();
    FeatureList nodeList=m_nodes.getWrappedList();
    final double nodeX = nodeLocation.getX();
    final double nodeY = nodeLocation.getY();
    final double searchWidthHalf=searchRectWidth/2; 
    
    GM_Position minPos = 
        GeometryFactory.createGM_Position( nodeX-searchWidthHalf, nodeY-searchWidthHalf );
    
    GM_Position maxPos =
      GeometryFactory.createGM_Position(nodeX+searchWidthHalf, nodeY+searchWidthHalf );
    
    GM_Envelope reqEnvelope = 
      GeometryFactory.createGM_Envelope( minPos, maxPos );
    foundNodes=nodeList.query(reqEnvelope,foundNodes);
    if(foundNodes.isEmpty())
    {
      System.out.println("Node not found Found:"+reqEnvelope);
      return null;
    }
    else
    {
      double min=Double.MAX_VALUE, curDist;
      IFE1D2DNode nearest=null, curNode; 
      for(Feature feature:foundNodes)
      {
        curNode=new FE1D2DNode(feature);
        curDist=nodeLocation.distance( curNode.getPoint() );
        if(min>curDist)
        {
          nearest=curNode;
          min=curDist;          
        }
      }
      return nearest;
    }
  }
}
