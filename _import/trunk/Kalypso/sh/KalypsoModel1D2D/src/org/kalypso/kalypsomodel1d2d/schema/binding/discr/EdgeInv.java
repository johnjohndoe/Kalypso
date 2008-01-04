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

import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.Util;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Just wrapped the an {@link IFE1D2DEdge} edge to signal it is inverted
 * 
 * @author Patrice Congo
 */
public class EdgeInv implements IEdgeInv
{
  private final IFE1D2DEdge edge;

  private final FeatureWrapperCollection<IFE1D2DElement> containers;

  /**
   * The inverted edge feature. the is only created on demand using {@link #addInvEdgeToElement(IFE1D2DElement)}
   */
  private Feature wrappedFeature;

  /**
   * Create a new edge which is the inverted of the given one.
   */
  public EdgeInv( final Feature invEdgeFeature )
  {
    Assert.throwIAEOnNull( invEdgeFeature, "invedgeFeature to wrapped must not be null" ); //$NON-NLS-1$
    Object toInv = invEdgeFeature.getProperty( Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_IN_INV );
    if( toInv instanceof String )
    {
      final GMLWorkspace workspace = invEdgeFeature.getWorkspace();
      toInv = workspace.getFeature( (String) toInv );
    }
    this.edge = (IFE1D2DEdge) ((Feature) toInv).getAdapter( IFE1D2DEdge.class );// new FE1D2DEdge((Feature)toInv);
    this.wrappedFeature = invEdgeFeature;
    containers = new FeatureWrapperCollection<IFE1D2DElement>( wrappedFeature, IFE1D2DElement.class, Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_CONTAINERS );
  }

  /**
   * Create a new edge which is the inverted of the given one.
   */
  public EdgeInv( final IFE1D2DEdge edge, final IFEDiscretisationModel1d2d targetModel )
  {
    Assert.throwIAEOnNull( edge, "edge to wrapped must not be null" ); //$NON-NLS-1$
    final Feature feature = edge.getWrappedFeature();
    Assert.throwIAEOnNotDirectInstanceOf( feature, IFE1D2DEdge.QNAME );

    // IRelationType parentRelation=feature.getParentRelation();
    // parentRelation.
    this.edge = edge;
    final IEdgeInv edgeInv = edge.getEdgeInv();
    if( edgeInv == null )
    {
      // this.wrappedFeature=null;
      final Feature modelFeature = targetModel.getWrappedFeature();
      Assert.throwIAEOnNull( modelFeature, "Model feature must not be null" ); //$NON-NLS-1$
      wrappedFeature = Util.createFeatureAsProperty( modelFeature, Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGES, Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE_INV );
      // set link to inverted
      wrappedFeature.setProperty( Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_IN_INV, edge.getGmlID() );
      final String edgeInvID = getGmlID();
      edge.setInvEdge( edgeInvID );
      for( final IFE1D2DNode node : (List<IFE1D2DNode>) edge.getNodes() )
      {
        node.addContainer( edgeInvID );
      }
    }
    else
    {
      wrappedFeature = edgeInv.getWrappedFeature();

    }

    containers = new FeatureWrapperCollection<IFE1D2DElement>( wrappedFeature, IFE1D2DElement.class, Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_CONTAINERS );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IEdgeInv#addInvEdgeToElement(org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement)
   */
  public void addInvEdgeToElement( final IFE1D2DElement targetElement )
  {
    throw new RuntimeException( "add inverted edge to element not in use " ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IEdgeInv#getInverted()
   */
  public IFE1D2DEdge getInverted( )
  {
    return edge;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#resetInvEdge()
   */
  public void resetInvEdge( )
  {
    throw new UnsupportedOperationException( "Reset inv edge for an inv edge does nnot make sense" ); //$NON-NLS-1$
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
    return containers;// edge.getContainers();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEEdge#getNodes()
   */
  public IFeatureWrapperCollection getNodes( )
  {
    throw new UnsupportedOperationException( "getting not in an inverted edge not allow" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper#getWrappedFeature()
   */
  public Feature getWrappedFeature( )
  {
    // throw new RuntimeException("do not use; instead use getInverted()");
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
    containerID = Assert.throwIAEOnNullOrEmpty( containerID );
    final FeatureList wrappedList = containers.getWrappedList();
    if( !wrappedList.contains( containerID ) )
      wrappedList.add( containerID );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#removeContainerAsRef(org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement)
   */
  public boolean removeContainerAsRef( final IFE1D2DElement containerToRemove )
  {
    Assert.throwIAEOnNullParam( containerToRemove, "containerToRemove" ); //$NON-NLS-1$
    final String id = containerToRemove.getGmlID();
    final FeatureList wrappedList = containers.getWrappedList();
    boolean hasBeenRemoved = false;
    while( wrappedList.remove( id ) )
    {
      hasBeenRemoved = true;
    }

    return hasBeenRemoved;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#getNode(int)
   */
  public IFE1D2DNode getNode( final int index ) throws IndexOutOfBoundsException
  {
    if( index > 1 )
      throw new IndexOutOfBoundsException( "index=" + index ); //$NON-NLS-1$
    return edge.getNode( 1 - index );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge#setNode(int,
   *      org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode)
   */
  public void setNode( final int index, final IFE1D2DNode node ) throws IndexOutOfBoundsException
  {
    if( index > 1 )
      throw new IndexOutOfBoundsException( "index=" + index ); //$NON-NLS-1$
    edge.setNode( 1 - index, node );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#addNode(java.lang.String)
   */
  public void addNode( final String nodeID )
  {
    throw new UnsupportedOperationException( "adding node to inv not supported" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getDescription()
   */
  public String getDescription( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getName()
   */
  public String getName( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setDescription(java.lang.String)
   */
  public void setDescription( final String desc )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setName(java.lang.String)
   */
  public void setName( final String name )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#setInvEdge(java.lang.String)
   */
  public void setInvEdge( final String invEdgeID )
  {
    throw new UnsupportedOperationException( "Cannot set inv to to a EdgeInv " ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#getEdgeInv()
   */
  public IEdgeInv getEdgeInv( )
  {
    throw new UnsupportedOperationException( "EdgeInv does not have an edgeinv " ); //$NON-NLS-1$
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final StringBuffer buf = new StringBuffer( 256 );
    buf.append( "EdgeInv" ); //$NON-NLS-1$
    buf.append( getGmlID() );
    buf.append( '[' );
    buf.append( edge );
    buf.append( ']' );

    return buf.toString();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#getMiddleNode()
   */
  public IFE1D2DNode getMiddleNode( )
  {
    return edge.getMiddleNode();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#setMiddleNode(org.kalypso.kalypsomodel1d2d.schema.binding.IFEMiddleNode)
   */
  public void setMiddleNode( final IFE1D2DNode middleNode )
  {
    edge.setMiddleNode( middleNode );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge#getMiddleNodePoint()
   */
  public GM_Point getMiddleNodePoint( )
  {
    return edge.getMiddleNodePoint();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge#getLeftElement()
   */
  public IFE1D2DElement getLeftElement( )
  {
    throw new UnsupportedOperationException( "Function not supported" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge#getRightElement()
   */
  public IFE1D2DElement getRightElement( )
  {
    throw new UnsupportedOperationException( "Function not supported" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge#recalculateMiddleNodePosition()
   */
  public void recalculateMiddleNodePosition( )
  {
    // edge.recalculateMiddleNodePosition();
  }

  /**
   * Always returns <code>false</code>, as inverted edges are never at the border...??? TODO: Check
   * 
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge#isBorder()
   */
  public boolean isBorder( )
  {
    return false;
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getLocation()
   */
  public GM_Object getLocation( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setLocation(org.kalypsodeegree.model.geometry.GM_Object)
   */
  public void setLocation( final GM_Object location )
  {
    throw new UnsupportedOperationException();
  }
}
