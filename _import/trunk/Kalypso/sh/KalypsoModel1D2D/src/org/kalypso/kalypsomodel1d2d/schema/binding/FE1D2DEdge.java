/**
 * 
 */
package org.kalypso.kalypsomodel1d2d.schema.binding;

import java.util.List;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.geom.ModelGeometryBuilder;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Gernot Belger
 */
public class FE1D2DEdge extends AbstractFeatureBinder 
            implements IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>
{
  private static final Logger logger=
        Logger.getLogger( FE1D2DEdge.class.toString() );
  
  private final IFeatureWrapperCollection<IFE1D2DElement> m_containers;

  private final IFeatureWrapperCollection<IFE1D2DNode> m_nodes;

  public FE1D2DEdge( final Feature featureToBind )
  {
    super( 
          featureToBind, 
          Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE );
    // containers
    Object prop=null;
    try
    {
      prop= 
        featureToBind.getProperty( 
            Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_CONTAINERS );
    }
    catch (Exception e) 
    {
      e.printStackTrace();
      logger.info( "feature:"+featureToBind);
    }
    if( prop == null )
    {
      // create the property tha is still missing
      //TODO check this since edge are not edge container this is not okay
      m_containers = 
          new FeatureWrapperCollection<IFE1D2DElement>( 
                      featureToBind, 
                      Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE, 
                      Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_CONTAINERS, 
                      IFE1D2DElement.class );
    }
    else
    {

      // just wrapped the existing one
      m_containers = 
          new FeatureWrapperCollection<IFE1D2DElement>( 
              featureToBind, IFE1D2DElement.class,// <IFE1D2DElement,IFE1D2DNode<IFE1D2DEdge>>.class,
              Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_CONTAINERS );
    }

    // nodes
    prop = featureToBind.getProperty( Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_CONTAINERS );
    if( prop == null )
    {
      // create the property tha is still missing
      m_nodes = new FeatureWrapperCollection<IFE1D2DNode>( featureToBind, Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE, Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDNODE, IFE1D2DNode.class );
    }
    else
    {

      // just wrapped the existing one
      m_nodes = new FeatureWrapperCollection<IFE1D2DNode>( featureToBind, IFE1D2DNode.class, Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDNODE );
    }
  }

  /**
   * This constructor creates {@link FE1D2DNode} based on a wb1d2d:FE1D2DNode feature which is created as child of the
   * given parent feaure and linked to it by the property of the type specified by the argument propQName.
   * 
   * @param parentFeature
   *          the parent feature for the new wbr:Roughness class
   * @param propQName
   *          the Q-name of the linking property type
   * @throws IllegalArgumentException
   *           if workspace is null or the roughness collection is not part of the workspace
   */
  public FE1D2DEdge( Feature parentFeature, QName propQName ) throws IllegalArgumentException
  {
    this( Util.createFeatureAsProperty( 
                parentFeature, 
                propQName, 
                Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE ) );
  }
  
  public FE1D2DEdge(
      Feature parentFeature,
      QName propQName,
      String gmlID)
  {
  this(
    org.kalypso.kalypsosimulationmodel.core.Util.createFeatureWithId( 
        Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE,
        parentFeature, 
        propQName, 
        gmlID ));
  }
  /**
   * Returns the (dereferenced) nodes of this egde. Elements of the array may be null.
   */
  public FE1D2DNode[] getNodesAsArray( )
  {
    final Feature feature = getFeature();
    final GMLWorkspace workspace = feature.getWorkspace();
    final List nodeList = 
          (List) feature.getProperty( 
              Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDNODE );

    final FE1D2DNode[] nodes = new FE1D2DNode[nodeList.size()];
    for( int i = 0; i < nodes.length; i++ )
    {
      /*
       * Accessing the list via index is ok here, because we should never have edges with more than 2 nodes.
       */
      final String ref = (String) nodeList.get( i );
      if( ref == null )
        nodes[i] = null;
      else
        nodes[i] = new FE1D2DNode( workspace.getFeature( ref ) );
    }

    return nodes;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEEdge#getNodes()
   */
  public IFeatureWrapperCollection<IFE1D2DNode> getNodes( )
  {
    return m_nodes;
  }

  /* static helper functions */
  public GM_Curve recalculateEgdeGeometry( ) throws GM_Exception
  {
//    final FE1D2DNode[] nodes = getNodesAsArray();
//    final GM_Position[] poses = new GM_Position[nodes.length];
//
//    if( nodes.length < 2 )
//      return null;
//
//    // REMARK: we assume here, that all nodes live in the same coordinate
//    // system.
//    final CS_CoordinateSystem crs = nodes[0].getPoint().getCoordinateSystem();
//
//    for( int i = 0; i < poses.length; i++ )
//    {
//      final GM_Point point = nodes[i].getPoint();
//      final GM_Position position = point.getPosition();
//      poses[i] = GeometryFactory.createGM_Position( position.getX(), position.getY() );
//    }
//
//    return GeometryFactory.createGM_Curve( poses, crs );
    return ModelGeometryBuilder.computeEgdeGeometry( this );
  }

  public static FE1D2DEdge createEdge( final FE1D2DDiscretisationModel discModel )
  {
    final Feature parentFeature = discModel.getFeature();
    final IFeatureType parentFT = parentFeature.getFeatureType();
    final IRelationType parentEdgeProperty = 
            (IRelationType) parentFT.getProperty( 
                Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGES );
    final IFeatureType edgeType = 
            parentFT.getGMLSchema().getFeatureType( 
                Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE /*QNAME_FE1D2DEdge*/ );
    final Feature edgeFeature = 
            parentFeature.getWorkspace().createFeature( 
                    parentFeature, parentEdgeProperty, edgeType );
    return new FE1D2DEdge( edgeFeature );
  }

  @SuppressWarnings("unchecked")
  public void setNodes( final FE1D2DNode node0, final FE1D2DNode node1 )
  {
    final Feature feature = getFeature();
    final List nodeList = 
      (List) feature.getProperty( 
          Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDNODE /*QNAME_PROP_DIRECTEDNODE*/ 
          );
    nodeList.clear();
    nodeList.add( node0.getFeature().getId() );
    nodeList.add( node1.getFeature().getId() );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEEdge#getContainers()
   */
  public IFeatureWrapperCollection<IFE1D2DElement> getContainers( )
  {
    return m_containers;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper#getWrappedFeature()
   */
  public Feature getWrappedFeature( )
  {
    return getFeature();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper#getGmlID()
   */
  public String getGmlID( )
  {
    return getFeature().getId();
  }
  
  public GM_Curve getCurve()
  {
    return (GM_Curve) getFeature().getProperty( 
                      Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_GEOM
                      /*QNAME_PROP_CURVE*/ );
  }
  
  public void setCurve(GM_Curve curve)
  {
    if(curve==null)
    {
      return;
    }
    //TODO allow prop setting vor this
    getFeature().setProperty( 
         Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_GEOM, 
         curve );
  }
  
  public void resetGeometry()
  {
    try
    {
      setCurve( recalculateEgdeGeometry() );
    }
    catch( GM_Exception e )
    {
      e.printStackTrace();
    }
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#addContainer(java.lang.String)
   */
  public void addContainer( String containerID )
  {
    Assert.throwIAEOnNullParam( containerID, "containerID" );
    FeatureList wrappedList=m_containers.getWrappedList();
    if(wrappedList.contains( containerID ))
    {
      logger.info( 
          "Edge container already registered as container:"+
          containerID );
    }
    else
    {
      wrappedList.add( containerID );
    }
  }
  
 /**
 * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#getNode(int)
 */
  public IFE1D2DNode getNode( int index ) throws IndexOutOfBoundsException
  {
    return m_nodes.get( index );
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge#addNode(java.lang.String)
   */
  public void addNode( String nodeID )
  {
    m_nodes.getWrappedList().add( nodeID );
  }
}
