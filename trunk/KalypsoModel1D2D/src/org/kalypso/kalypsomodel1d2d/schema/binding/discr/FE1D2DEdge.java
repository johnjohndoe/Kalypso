/**
 *
 */
package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.geom.ModelGeometryBuilder;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Gernot Belger
 */
public class FE1D2DEdge extends Feature_Impl implements IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>
{
  private final IFeatureBindingCollection<IFE1D2DElement> m_containers = new FeatureBindingCollection<IFE1D2DElement>( this, IFE1D2DElement.class, WB1D2D_PROP_EDGE_CONTAINERS );

  protected final IFeatureBindingCollection<IFE1D2DNode> m_nodes = new FeatureBindingCollection<IFE1D2DNode>( this, IFE1D2DNode.class, WB1D2D_PROP_DIRECTEDNODE );

  public FE1D2DEdge( Object parent, IRelationType parentRelation, IFeatureType ft, String id, Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public static final IFE1D2DEdge createFromModel( final IFEDiscretisationModel1d2d model, final IFE1D2DNode node0, final IFE1D2DNode node1 )
  {
    final IFeatureBindingCollection<IFE1D2DEdge> edges = model.getEdges();
    final IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> curEdge = edges.addNew( IFE1D2DEdge.QNAME );
    final String edgeGmlID = curEdge.getId();
    curEdge.addNode( node0.getId() );
    node0.addContainer( edgeGmlID );
    //
    curEdge.addNode( node1.getId() );
    node1.addContainer( edgeGmlID );

    curEdge.invalidEnvelope();

    return curEdge;

  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEEdge#getNodes()
   */
  @Override
  public IFeatureBindingCollection<IFE1D2DNode> getNodes( )
  {
    return m_nodes;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#getMiddleNode()
   */
  @Override
  public IFE1D2DNode getMiddleNode( )
  {
    if( getProperty( IFE1D2DEdge.WB1D2D_PROP_MIDDLE_NODE ) != null )
    {
      final Feature middleNodeFeature = FeatureHelper.getSubFeature( this, IFE1D2DEdge.WB1D2D_PROP_MIDDLE_NODE );
      if( middleNodeFeature == null )
        return null;
      else
        return (IFE1D2DNode) middleNodeFeature.getAdapter( IFE1D2DNode.class );
    }
    return null;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#setMiddleNode(org.kalypso.kalypsomodel1d2d.schema.binding.IFEMiddleNode)
   */
  @Override
  public void setMiddleNode( final IFE1D2DNode middleNode )
  {
    String newMiddleNodeID = null;
    if( middleNode != null )
    {
      newMiddleNodeID = middleNode.getId();
    }
    setProperty( IFE1D2DEdge.WB1D2D_PROP_MIDDLE_NODE, newMiddleNodeID );
  }

  /* static helper functions */
  @Override
  public GM_Curve recalculateElementGeometry( ) throws GM_Exception
  {
    return ModelGeometryBuilder.computeEgdeGeometry( this );
  }

  public static IFE1D2DEdge createEdge( final IFEDiscretisationModel1d2d discModel )
  {
    return discModel.getEdges().addNew( IFE1D2DEdge.QNAME );
  }

  @SuppressWarnings("unchecked")
  public void setNodes( final IFE1D2DNode node0, final IFE1D2DNode node1 )
  {
    final FeatureList nodeList = (FeatureList) getProperty( IFE1D2DEdge.WB1D2D_PROP_DIRECTEDNODE );
    nodeList.clear();
    nodeList.add( node0.getId() );
    nodeList.add( node1.getId() );
    nodeList.invalidate();
    invalidEnvelope();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEEdge#getContainers()
   */
  @Override
  public IFeatureBindingCollection<IFE1D2DElement> getContainers( )
  {
    return m_containers;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#addContainer(java.lang.String)
   */
  @Override
  public void addContainer( final String containerID )
  {
    Assert.throwIAEOnNullParam( containerID, "containerID" ); //$NON-NLS-1$
    final FeatureList wrappedList = m_containers.getFeatureList();
    if( wrappedList.contains( containerID ) )
    {
      return;
    }
    else
    {
      wrappedList.add( containerID );
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#getNode(int)
   */
  @Override
  public IFE1D2DNode getNode( final int index ) throws IndexOutOfBoundsException
  {
    return m_nodes.get( index );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#addNode(java.lang.String)
   */
  @Override
  public void addNode( final String nodeID )
  {
    final FeatureList wrappedList = m_nodes.getFeatureList();
    wrappedList.add( nodeID );

    // changeing the nodes invalidates my geometry
    wrappedList.invalidate();
    invalidEnvelope();
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final StringBuffer buf = new StringBuffer( 256 );
    buf.append( this );
    buf.append( '[' );
    for( final IFE1D2DNode node : m_nodes )
    {
      buf.append( node );
      buf.append( ' ' );
    }
    buf.append( ']' );
    return buf.toString();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge#getMiddleNodePoint()
   */
  @Override
  public GM_Point getMiddleNodePoint( )
  {
    final GM_Point point1 = m_nodes.get( 0 ).getPoint();
    final GM_Point point2 = m_nodes.get( 1 ).getPoint();
    final double x = (point1.getX() + point2.getX()) / 2;
    final double y = (point1.getY() + point2.getY()) / 2;
    if( point1.getCoordinateDimension() > 2 && point2.getCoordinateDimension() > 2 )
    {
      final double z = (point1.getZ() + point2.getZ()) / 2;
      return GeometryFactory.createGM_Point( x, y, z, point1.getCoordinateSystem() );
    }
    else
      return GeometryFactory.createGM_Point( x, y, point1.getCoordinateSystem() );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge#getLeftElement()
   */
  @Override
  public IFeatureBindingCollection<IFE1D2DElement> getAdjacentElements( )
  {
    return getContainers();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge#isBorder()
   */
  @Override
  public boolean isBorder( )
  {
    final IFeatureBindingCollection<IFE1D2DElement> containers = getContainers();
    return containers.size() == 1;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge#getGeometry()
   */
  @Override
  public GM_Curve getGeometry( )
  {
    return getProperty( WB1D2D_PROP_MIDDLE_GEOM, GM_Curve.class );
  }

}
