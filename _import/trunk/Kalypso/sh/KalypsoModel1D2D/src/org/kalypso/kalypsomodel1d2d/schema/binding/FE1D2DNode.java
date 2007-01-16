package org.kalypso.kalypsomodel1d2d.schema.binding;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * @author Gernot Belger
 */
public class FE1D2DNode extends AbstractFeatureBinder implements IFE1D2DNode<IFE1D2DEdge>
{
  public final static QName QNAME_FE1D2DNode = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "FE1D2DNode" );

  public final static QName QNAME_PROP_POINT = new QName( NS.GML3, "pointProperty" );

  public final static QName QNAME_PROP_NODE_CONTAINERS = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "fe1d2dNodeContainer" );

  // TODO check remove of discretisation model
  // private final FE1D2DDiscretisationModel m_discretisationModel;
  private final FeatureWrapperCollection<IFE1D2DEdge> containers;

  public FE1D2DNode( final Feature featureToBind )
  {
    super( featureToBind, QNAME_FE1D2DNode );

    // m_discretisationModel = new FE1D2DDiscretisationModel( getFeature().getParent() );
    //
    Object prop = featureToBind.getProperty( QNAME_PROP_NODE_CONTAINERS );

    if( prop == null )
    {
      // create the property tha is still missing
      containers = new FeatureWrapperCollection<IFE1D2DEdge>( featureToBind, Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DNODE, QNAME_PROP_NODE_CONTAINERS, IFE1D2DEdge.class );
    }
    else
    {
      // just wrapped the existing one
      containers = new FeatureWrapperCollection<IFE1D2DEdge>( featureToBind, IFE1D2DEdge.class,// <IFE1D2DElement,IFE1D2DNode<IFE1D2DEdge>>.class,
      QNAME_PROP_NODE_CONTAINERS );
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
  public FE1D2DNode( Feature parentFeature, QName propQName ) throws IllegalArgumentException
  {
    this( Util.createFeatureAsProperty( parentFeature, propQName, Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2DNODE ) );
  }

  public GM_Point getPoint( )
  {
    return (GM_Point) getFeature().getProperty( QNAME_PROP_POINT );
  }

  public void setPoint( final GM_Point point )
  {
    getFeature().setProperty( QNAME_PROP_POINT, point );
  }

  public static FE1D2DNode createNode( final FE1D2DDiscretisationModel discModel )
  {
    final Feature parentFeature = discModel.getFeature();
    final IFeatureType parentFT = parentFeature.getFeatureType();
    final IRelationType parentNodeProperty = (IRelationType) parentFT.getProperty( FE1D2DDiscretisationModel.QNAME_PROP_NODES );
    final IFeatureType nodeType = parentFT.getGMLSchema().getFeatureType( QNAME_FE1D2DNode );
    final Feature nodeFeature = parentFeature.getWorkspace().createFeature( parentFeature, parentNodeProperty, nodeType );
    return new FE1D2DNode( nodeFeature );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFENode#getContainers()
   */
  public IFeatureWrapperCollection<IFE1D2DEdge> getContainers( )
  {
    return containers;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper#getWrappedFeature()
   */
  public Feature getWrappedFeature( )
  {
    return getFeature();
  }

  /**
   * Returns all elements, this node is part of.
   */
  public IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>[] getElements( )
  {
    // TODO: at the moment, the elements are found via the geometric position, maybe change this later to references via
    // the containers.

    final FE1D2DDiscretisationModel model = new FE1D2DDiscretisationModel( getFeature().getParent() );
    final FeatureList elementList = (FeatureList) model.getFeature().getProperty( FE1D2DDiscretisationModel.QNAME_PROP_ELEMENTS );

    // get all elements touching this node
    final List touchingElements = elementList.query( getPoint().getPosition(), null );

    final List<IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>> foundElements = new ArrayList<IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>>();

    // filter all element which contain this node
    for( final Object object : touchingElements )
    {
      final Feature f = (Feature) object;
      final IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge> elt = (IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>) f.getAdapter( IFE1D2DElement.class );
      final IFeatureWrapperCollection<IFE1D2DEdge> edges = elt.getEdges();
      for( final IFE1D2DEdge edge : edges )
      {
        final IFeatureWrapperCollection nodes = edge.getNodes();
        if( nodes.contains( this ) )
        {
          foundElements.add( elt );
          break;
        }
      }
    }

    final IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>[] result = new IFE1D2DElement[foundElements.size()];
    for( int i = 0; i < foundElements.size(); i++ )
      result[i] = (IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>) foundElements.get( i );

    return result;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode#getEdges()
   */
  public IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>[] getEdges( )
  {
    // TODO: at the moment, the elements are found via the geometric position, maybe change this later to references via
    // the containers.

    final FE1D2DDiscretisationModel model = new FE1D2DDiscretisationModel( getFeature().getParent() );
    final FeatureList edgeList = (FeatureList) model.getFeature().getProperty( FE1D2DDiscretisationModel.QNAME_PROP_EDGES );

    // get all elements touching this node
    final List touchingEdges = edgeList.query( this.getPoint().getPosition(), null );

    final List<IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>> foundEdges = new ArrayList<IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>>();

    // filter all element which contain this node
    for( final Object object : touchingEdges )
    {
      final Feature f = (Feature) object;
      final IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge = (IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>) f.getAdapter( IFE1D2DEdge.class );
      final IFeatureWrapperCollection<IFE1D2DNode> nodes = edge.getNodes();
      for( final IFE1D2DNode node : nodes )
      {
        if( node.equals( this ) )
        {
          foundEdges.add( edge );
          break;
        }
      }
    }

    final IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>[] result = new IFE1D2DEdge[foundEdges.size()];
    for( int i = 0; i < foundEdges.size(); i++ )
      result[i] = (IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>) foundEdges.get( i );

    return result;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode#getNeighbours()
   */
  public IFE1D2DNode<IFE1D2DEdge>[] getNeighbours( )
  {
    final IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>[] elements = getElements();

    final Set<IFE1D2DNode<IFE1D2DEdge>> neighbourNodes = new HashSet<IFE1D2DNode<IFE1D2DEdge>>();

    // alle Nachbarknoten
    for( final IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge> element : elements )
    {
      final IFeatureWrapperCollection<IFE1D2DEdge> edges = element.getEdges();
      for( final IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge : edges )
      {
        for( final IFE1D2DNode<IFE1D2DEdge> node : edge.getNodes() )
        {
          if( !equals( node ) )
            neighbourNodes.add( node );
        }
      }
    }

    return neighbourNodes.toArray( new IFE1D2DNode[neighbourNodes.size()] );
  }

}
