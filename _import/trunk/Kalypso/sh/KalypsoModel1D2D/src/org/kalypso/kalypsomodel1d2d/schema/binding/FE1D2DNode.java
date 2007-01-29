package org.kalypso.kalypsomodel1d2d.schema.binding;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.namespace.QName;


import org.deegree.gml.GMLSchema;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * The default implementation of {@link IFE1D2DNode} based on 
 * {@link AbstractFeatureBinder} to bind wb1d2d:Node elements
 * 
 * @author Gernot Belger, Patrice Congo
 */
public class FE1D2DNode 
                extends AbstractFeatureBinder 
                implements IFE1D2DNode<IFE1D2DEdge>
{
  /** Edges that contains this node*/
  private final FeatureWrapperCollection<IFE1D2DEdge> containers;
  
 
  /**
   * Creates a new node object that binds the given feature.
   * 
   * @param featureToBind the feature to bind
   * @see FE1D2DNode#FE1D2DNode(Feature, QName)
   * @throws IllegalArgumentException if the passed feature does not
   *  substitutes to wb1d2d:Node
   */
  public FE1D2DNode( final Feature featureToBind )
  {
    super( 
          featureToBind, 
          Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );

    
    Object prop = 
      featureToBind.getProperty( 
            Kalypso1D2DSchemaConstants.WB1D2D_PROP_NODE_CONTAINERS);

    if( prop == null )
    {
      // create the property tha is still missing
      containers = 
            new FeatureWrapperCollection<IFE1D2DEdge>( 
                featureToBind, 
                Kalypso1D2DSchemaConstants.WB1D2D_F_NODE, 
                Kalypso1D2DSchemaConstants.WB1D2D_PROP_NODE_CONTAINERS/*QNAME_PROP_NODE_CONTAINERS*/, 
                IFE1D2DEdge.class );
    }
    else
    {
      // just wrapped the existing one
      containers = 
            new FeatureWrapperCollection<IFE1D2DEdge>( 
                featureToBind, 
                IFE1D2DEdge.class,// <IFE1D2DElement,IFE1D2DNode<IFE1D2DEdge>>.class,
                Kalypso1D2DSchemaConstants.WB1D2D_PROP_NODE_CONTAINERS/*QNAME_PROP_NODE_CONTAINERS*/ );
    }
  }

  /**
   * This constructor creates {@link FE1D2DNode} based on a wb1d2d:Node feature 
   * which is created as child of the given parent feaure and 
   * linked to it by the property of the type specified by the 
   * argument propQName.
   * 
   * @param parentFeature
   *          the parent feature for the new wbr:Roughness class
   * @param propQName
   *          the Q-name of the linking property type
   * @throws IllegalArgumentException
   *           if workspace is null or the roughness collection is not 
   *           part of the workspace
   */
  public FE1D2DNode( 
              Feature parentFeature, 
              QName propQName ) 
              throws IllegalArgumentException
  {
    this( 
        Util.createFeatureAsProperty( 
                  parentFeature, 
                  propQName, 
                  Kalypso1D2DSchemaConstants.WB1D2D_F_NODE ) );
  }
  
  //TODO implements this constructor
  /**
   * Creates a feature with this gml id.
   * This 
   */
  public FE1D2DNode(
            Feature parentFeature,
            QName propQName,
            String gmlID)
  {
    this(
        createNodeById( 
              parentFeature, propQName, gmlID ));
  }

  private static final Feature createNodeById(
                      Feature parentFeature,
                      QName propQName,
                      String gmlID)
                      throws IllegalArgumentException
  {
    Assert.throwIAEOnNull( 
            parentFeature, 
            "parentFeatureParameter must not be null" );
    Assert.throwIAEOnNull( 
            propQName, "propQName param must notbe null" );
    gmlID=Assert.throwIAEOnNullOrEmpty( gmlID );
    
    GMLWorkspace workspace=parentFeature.getWorkspace();
    IGMLSchema schema=workspace.getGMLSchema();
    IFeatureType featureType=
      schema.getFeatureType(
          Kalypso1D2DSchemaConstants.WB1D2D_F_NODE);
    IPropertyType parentPT=
         parentFeature.getFeatureType().getProperty( propQName );
    if(!(parentPT instanceof      IRelationType))
    {
      throw new IllegalArgumentException(
                "Property not a IRelationType="+parentPT+
                " propQname="+propQName);
    }
    
    Feature created=
      FeatureFactory.createFeature( 
        parentFeature, 
        (IRelationType)parentPT, 
        gmlID, 
        featureType, 
        true );
    try
    {
      if(parentPT.isList())
      {
        workspace.addFeatureAsComposition( 
              parentFeature, 
              (IRelationType)parentPT, 
              -1, 
              created );
      }
      else
      {
        //TODO test this case
        parentFeature.setProperty( parentPT, created );
      }
    }
    catch( Exception e )
    {
      throw new RuntimeException("Could not add to the workspace",e);
    }
    
    return created;
  }
  
  public GM_Point getPoint( )
  {
    return (GM_Point) m_featureToBind.getProperty( 
              Kalypso1D2DSchemaConstants.WB1D2D_PROP_POINT);
  }

  public void setPoint( final GM_Point point )
  {
    m_featureToBind.setProperty( 
        Kalypso1D2DSchemaConstants.WB1D2D_PROP_POINT/*QNAME_PROP_POINT*/, 
        point );
  }

  public static FE1D2DNode createNode( final FE1D2DDiscretisationModel discModel )
  {
    final Feature parentFeature = discModel.getFeature();
    final IFeatureType parentFT = parentFeature.getFeatureType();
    final IRelationType parentNodeProperty = 
            (IRelationType) parentFT.getProperty( 
                Kalypso1D2DSchemaConstants.WB1D2D_PROP_NODES
                /*FE1D2DDiscretisationModel.QNAME_PROP_NODES*/ );
    final IFeatureType nodeType = 
      parentFT.getGMLSchema().getFeatureType( 
              Kalypso1D2DSchemaConstants.WB1D2D_F_NODE  );
    final Feature nodeFeature = 
      parentFeature.getWorkspace().createFeature( 
              parentFeature, parentNodeProperty, nodeType );
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
    return m_featureToBind;
  }

  /**
   * Returns all elements, this node is part of.
   */
  public IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>[] getElements( )
  {
    // TODO: at the moment, the elements are found via the geometric position, maybe change this later to references via
    // the containers.

    final FE1D2DDiscretisationModel model = new FE1D2DDiscretisationModel( getFeature().getParent() );
    final FeatureList elementList = 
          (FeatureList) model.getFeature().getProperty( 
                Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENTS );

    // get all elements touching this node
    final List touchingElements = 
        elementList.query( getPoint().getPosition(), null );

    final List<IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>> foundElements = 
                new ArrayList<IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>>();

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
    final FeatureList edgeList = 
          (FeatureList) model.getFeature().getProperty( 
              Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGES );

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
