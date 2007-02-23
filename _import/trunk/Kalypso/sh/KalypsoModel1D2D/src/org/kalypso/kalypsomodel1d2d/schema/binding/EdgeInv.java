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


import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;

/**
 * Just wrapped the an {@link IFE1D2DEdge} edge to signal 
 * it is inverted 
 * 
 * @author Patrice Congo
 */
public class EdgeInv implements IEdgeInv
{
  private final IFE1D2DEdge edge;
  private final FeatureWrapperCollection<IFE1D2DElement> containers;
  
  /**
   * The inverted edge feature. the is only created on demand
   * using {@link #addInvEdgeToElement(IFE1D2DElement)}
   */
  private Feature wrappedFeature;
  
//  /**
//   * Create a new edge for the given edge
//   * and linked it the given parentFeature
//   * @param edgeToInv 
//   * @param parentFeature 
//   * @param propQName  
//   */
//  private EdgeInv( 
//        Feature edgeToInv, 
//        Feature parentFeature,
//        QName propQName)
//  {
//    wrappedFeature=
//        Util.createFeatureAsProperty( 
//            parentFeature, 
//            propQName, 
//            Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE_INV );
//    wrappedFeature.setProperty( 
//        Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_IN_INV, 
//        edgeToInv.getId() );
//    edge=(IFE1D2DEdge)edgeToInv.getAdapter( IFE1D2DEdge.class);
//    containers = 
//      new FeatureWrapperCollection<IFE1D2DElement>( 
//                  wrappedFeature,//featureToBind, 
//                  Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE, 
//                  Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_CONTAINERS, 
//                  IFE1D2DElement.class );
//  }
  
//  /**
//   * Create a new edge which is the inverted of the given one.
//   */
//  private EdgeInv( IFE1D2DEdge edge)
//  {
//    Assert.throwIAEOnNull( edge, "edge to wrapped must not be null" );
//    Feature feature=edge.getWrappedFeature();
//    Assert.throwIAEOnNotDirectInstanceOf( 
//          feature, Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE );
//    
////    IRelationType parentRelation=feature.getParentRelation();
////    parentRelation.
//    this.edge=edge;
//    this.wrappedFeature=null;
////    wrappedFeature=
////    Util.createFeatureAsProperty( 
////        modelFeature, 
////        Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGES, 
////        Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE_INV );
////  //set link to inverted
////  wrappedFeature.setProperty( 
////      Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_IN_INV, 
////      edge.getWrappedFeature().getId() );
//  }
  
  /**
   * Create a new edge which is the inverted of the given one.
   */
  public EdgeInv( Feature invEdgeFeature)
  {
    Assert.throwIAEOnNull( 
        invEdgeFeature, "invedgeFeature to wrapped must not be null" );
//    Feature invertedFeature=null;
//      (Feature)invEdgeFeature.getProperty( 
//                  Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_IN_INV );
    Object toInv=
      invEdgeFeature.getProperty( 
            Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_IN_INV );
    if(toInv instanceof String)
    {
      GMLWorkspace workspace=invEdgeFeature.getWorkspace();
      toInv=workspace.getFeature( (String )toInv);
    }
    this.edge = new FE1D2DEdge((Feature)toInv);
    
//    Assert.throwIAEOnNotDirectInstanceOf( 
//          invertedFeature, Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE );
    
//    IRelationType parentRelation=feature.getParentRelation();
//    parentRelation.
    this.wrappedFeature=invEdgeFeature;
    containers = 
      new FeatureWrapperCollection<IFE1D2DElement>(
          wrappedFeature,
          IFE1D2DElement.class,
          Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_CONTAINERS);
//      new FeatureWrapperCollection<IFE1D2DElement>( 
//                  wrappedFeature, 
//                  Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT,//DGE_INV, 
//                  Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_CONTAINERS, 
//                  IFE1D2DElement.class );
  }
  
  /**
   * Create a new edge which is the inverted of the given one.
   */
  public EdgeInv( IFE1D2DEdge edge, 
                  IFEDiscretisationModel1d2d targetModel)
  {
    Assert.throwIAEOnNull( edge, "edge to wrapped must not be null" );
    Feature feature=edge.getWrappedFeature();
    Assert.throwIAEOnNotDirectInstanceOf( 
          feature, Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE );
    
//    IRelationType parentRelation=feature.getParentRelation();
//    parentRelation.
    this.edge=edge;
//    this.wrappedFeature=null;
    Feature modelFeature=targetModel.getWrappedFeature();
    Assert.throwIAEOnNull( modelFeature, "Model feature must not be null" );
    wrappedFeature=
        Util.createFeatureAsProperty( 
            modelFeature, 
            Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGES, 
            Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE_INV );
    //set link to inverted
    wrappedFeature.setProperty( 
        Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_IN_INV, 
        edge.getGmlID() );
    containers = 
        new FeatureWrapperCollection<IFE1D2DElement>( 
            wrappedFeature,//featureToBind, 
            IFE1D2DElement.class,// <IFE1D2DElement,IFE1D2DNode<IFE1D2DEdge>>.class,
            Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_CONTAINERS );
//      new FeatureWrapperCollection<IFE1D2DElement>( 
//                  wrappedFeature,//featureToBind, 
//                  Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE_INV, 
//                  Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_CONTAINERS, 
//                  IFE1D2DElement.class );
    
  }
  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IEdgeInv#addInvEdgeToElement(org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement)
   */
  public void addInvEdgeToElement( 
                IFE1D2DElement targetElement)
  {
//    Feature modelFeature=targetModel.getWrappedFeature();
//    Assert.throwIAEOnNull( modelFeature, "Model feature must not be null" );
//    wrappedFeature=
//      Util.createFeatureAsProperty( 
//          modelFeature, 
//          Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGES, 
//          Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE_INV );
////  set link to inverted
//    wrappedFeature.setProperty( 
//        Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_IN_INV, 
//        edge.getWrappedFeature().getId() );
//    
//    //add to element
//    Feature elementFeature=targetElement.getWrappedFeature();
//    Assert.throwIAEOnNull( elementFeature, "Element feature must not be null" );
//    
    throw new RuntimeException("add inverted edge to element not in use ");
    
    
  }
  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IEdgeInv#getInverted()
   */
  public IFE1D2DEdge getInverted( )
  {
    return edge;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#getCurve()
   */
  public GM_Curve getCurve( )
  {
    return edge.getCurve();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEEdge#getContainers()
   */
  public IFeatureWrapperCollection getContainers( )
  {
    return edge.getContainers();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEEdge#getNodes()
   */
  public IFeatureWrapperCollection getNodes( )
  {
    throw new UnsupportedOperationException(
        "getting not in an inverted edge not allow");
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper#getWrappedFeature()
   */
  public Feature getWrappedFeature( )
  {
//    throw new RuntimeException("do not use; instead use getInverted()");
    return wrappedFeature;
  }
  
  /**
   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper#getGmlID()
   */
  public String getGmlID( )
  {
    return wrappedFeature.getId();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#addContainer(java.lang.String)
   */
  public void addContainer( String containerID )
  {
    Assert.throwIAEOnNullParam( containerID, "containerID" );
    containers.getWrappedList().add( containerID );
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#getNode(int)
   */
  public IFE1D2DNode getNode( int index ) throws IndexOutOfBoundsException
  {
    if(index>1)
    {
      throw new IndexOutOfBoundsException("index="+index);
    }
    return edge.getNode( 1-index );
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#addNode(java.lang.String)
   */
  public void addNode( String nodeID )
  {
    throw new UnsupportedOperationException(
                      "adding node to inv not supported");
  }

  // TODO: The methods below should better be implemented by extending 'AbstractFeatureBinder'
  // Sadly, this is not so easily possible because of the second constructor of this class...
  
  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getDescription()
   */
  public String getDescription( )
  {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getName()
   */
  public String getName( )
  {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setDescription(java.lang.String)
   */
  public void setDescription( String desc )
  {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setName(java.lang.String)
   */
  public void setName( String name )
  {
    // TODO Auto-generated method stub
    throw new UnsupportedOperationException();
  }
}
