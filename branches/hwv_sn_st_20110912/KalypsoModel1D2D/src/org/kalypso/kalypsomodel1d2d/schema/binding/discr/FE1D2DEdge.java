/**
 *
 */
package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.logging.Logger;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.geom.ModelGeometryBuilder;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Gernot Belger
 */
public class FE1D2DEdge extends AbstractFeatureBinder implements IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>
{
  private static final Logger logger = Logger.getLogger( FE1D2DEdge.class.toString() );

  private final IFeatureWrapperCollection<IFE1D2DElement> m_containers;

  private final IFeatureWrapperCollection<IFE1D2DNode> m_nodes;

  public FE1D2DEdge( final Feature featureToBind )
  {
    super( featureToBind, IFE1D2DEdge.QNAME );
    // containers
    Object prop = null;
    try
    {
      prop = featureToBind.getProperty( IFE1D2DEdge.WB1D2D_PROP_EDGE_CONTAINERS );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      logger.info( "feature:" + featureToBind ); //$NON-NLS-1$
    }
    if( prop == null )
    {
      // create the property that is still missing
      // TODO check this since edge are not edge container this is not okay
      m_containers = new FeatureWrapperCollection<IFE1D2DElement>( featureToBind, IFE1D2DEdge.QNAME, IFE1D2DEdge.WB1D2D_PROP_EDGE_CONTAINERS, IFE1D2DElement.class );
    }
    else
    {
      // just wrapped the existing one
      m_containers = new FeatureWrapperCollection<IFE1D2DElement>( featureToBind, IFE1D2DElement.class, IFE1D2DEdge.WB1D2D_PROP_EDGE_CONTAINERS );
    }

    // nodes
    prop = featureToBind.getProperty( IFE1D2DEdge.WB1D2D_PROP_EDGE_CONTAINERS );
    if( prop == null )
    {
      // create the property that is still missing
      m_nodes = new FeatureWrapperCollection<IFE1D2DNode>( featureToBind, IFE1D2DEdge.QNAME, IFE1D2DEdge.WB1D2D_PROP_DIRECTEDNODE, IFE1D2DNode.class );
    }
    else
    {
      // just wrapped the existing one
      m_nodes = new FeatureWrapperCollection<IFE1D2DNode>( featureToBind, IFE1D2DNode.class, IFE1D2DEdge.WB1D2D_PROP_DIRECTEDNODE );
    }
  }

  public static final IFE1D2DEdge createFromModel( final IFEDiscretisationModel1d2d model, final IFE1D2DNode node0, final IFE1D2DNode node1 )
  {
    final IFeatureWrapperCollection<IFE1D2DEdge> edges = model.getEdges();
    final IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> curEdge = edges.addNew( IFE1D2DEdge.QNAME, IFE1D2DEdge.class );
    final String edgeGmlID = curEdge.getGmlID();
    curEdge.addNode( node0.getGmlID() );
    node0.addContainer( edgeGmlID );
    //
    curEdge.addNode( node1.getGmlID() );
    node1.addContainer( edgeGmlID );

    curEdge.getFeature().invalidEnvelope();

    return curEdge;

  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEEdge#getNodes()
   */
  @Override
  public IFeatureWrapperCollection<IFE1D2DNode> getNodes( )
  {
    return m_nodes;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#getMiddleNode()
   */
  @Override
  public IFE1D2DNode getMiddleNode( )
  {
    if( getFeature().getProperty( IFE1D2DEdge.WB1D2D_PROP_MIDDLE_NODE ) != null )
    {
      final Feature middleNodeFeature = FeatureHelper.getSubFeature( getFeature(), IFE1D2DEdge.WB1D2D_PROP_MIDDLE_NODE );
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
      newMiddleNodeID = middleNode.getGmlID();
    }
    getFeature().setProperty( IFE1D2DEdge.WB1D2D_PROP_MIDDLE_NODE, newMiddleNodeID );
  }

  /* static helper functions */
  public GM_Curve recalculateEgdeGeometry( ) throws GM_Exception
  {
    return ModelGeometryBuilder.computeEgdeGeometry( this );
  }

  public static FE1D2DEdge createEdge( final IFEDiscretisationModel1d2d discModel )
  {
    final Feature parentFeature = discModel.getFeature();
    final IFeatureType parentFT = parentFeature.getFeatureType();
    final IRelationType parentEdgeProperty = (IRelationType) parentFT.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_EDGES );
    final IFeatureType edgeType = parentFT.getGMLSchema().getFeatureType( IFE1D2DEdge.QNAME );
    final Feature edgeFeature = parentFeature.getWorkspace().createFeature( parentFeature, parentEdgeProperty, edgeType );
    return new FE1D2DEdge( edgeFeature );
  }

  @SuppressWarnings("unchecked")
  public void setNodes( final IFE1D2DNode node0, final IFE1D2DNode node1 )
  {
    final Feature feature = getFeature();
    final FeatureList nodeList = (FeatureList) feature.getProperty( IFE1D2DEdge.WB1D2D_PROP_DIRECTEDNODE /* QNAME_PROP_DIRECTEDNODE */
    );
    nodeList.clear();
    nodeList.add( node0.getFeature().getId() );
    nodeList.add( node1.getFeature().getId() );
    nodeList.invalidate();
    getFeature().invalidEnvelope();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEEdge#getContainers()
   */
  @Override
  public IFeatureWrapperCollection<IFE1D2DElement> getContainers( )
  {
    return m_containers;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper#getGmlID()
   */
  @Override
  public String getGmlID( )
  {
    return getFeature().getId();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#addContainer(java.lang.String)
   */
  @Override
  public void addContainer( final String containerID )
  {
    Assert.throwIAEOnNullParam( containerID, "containerID" ); //$NON-NLS-1$
    final FeatureList wrappedList = m_containers.getWrappedList();
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
    final FeatureList wrappedList = m_nodes.getWrappedList();
    wrappedList.add( nodeID );

    // changeing the nodes invalidates my geometry
    wrappedList.invalidate();
    getFeature().invalidEnvelope();
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final StringBuffer buf = new StringBuffer( 256 );
    buf.append( getFeature() );
    buf.append( '[' );
    for( final IFE1D2DNode node : m_nodes )
    {
      buf.append( node.getFeature() );
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
  public IFeatureWrapperCollection<IFE1D2DElement> getAdjacentElements( )
  {
    return getContainers();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge#isBorder()
   */
  @Override
  public boolean isBorder( )
  {
    final IFeatureWrapperCollection<IFE1D2DElement> containers = getContainers();
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
