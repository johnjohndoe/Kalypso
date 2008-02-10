package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * The default implementation of {@link IFE1D2DNode} based on {@link AbstractFeatureBinder} to bind wb1d2d:Node elements
 * 
 * @author Gernot Belger, Patrice Congo
 */
public class FE1D2DNode extends AbstractFeatureBinder implements IFE1D2DNode
{
  /** Edges that contains this node */
  private final FeatureWrapperCollection m_containers;

  /**
   * Creates a new node object that binds the given feature.
   * 
   * @param featureToBind
   *            the feature to bind
   * @see FE1D2DNode#FE1D2DNode(Feature, QName)
   * @throws IllegalArgumentException
   *             if the passed feature does not substitutes to wb1d2d:Node
   */
  public FE1D2DNode( final Feature featureToBind )
  {
    super( featureToBind, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );

    final Object prop = featureToBind.getProperty( IFE1D2DNode.WB1D2D_PROP_NODE_CONTAINERS );

    if( prop == null )
    {
      // create the property that is still missing
      m_containers = new FeatureWrapperCollection( featureToBind, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE, IFE1D2DNode.WB1D2D_PROP_NODE_CONTAINERS, IFE1D2DEdge.class );
    }
    else
    {
      // just wrapped the existing one
      m_containers = new FeatureWrapperCollection( featureToBind, IFE1D2DEdge.class, IFE1D2DNode.WB1D2D_PROP_NODE_CONTAINERS );
    }
    m_containers.addSecondaryWrapper( IFELine.class );
    // TODO: does this collection really may contain 1D-Elements??
    m_containers.addSecondaryWrapper( IElement1D.class );
  }

  /**
   * This constructor creates {@link FE1D2DNode} based on a wb1d2d:Node feature which is created as child of the given
   * parent feaure and linked to it by the property of the type specified by the argument propQName.
   * 
   * @param parentFeature
   *            the parent feature for the new wbr:Roughness class
   * @param propQName
   *            the Q-name of the linking property type
   * @throws IllegalArgumentException
   *             if workspace is null or the roughness collection is not part of the workspace
   */
  public FE1D2DNode( final Feature parentFeature, final QName propQName ) throws IllegalArgumentException
  {
    this( Util.createFeatureAsProperty( parentFeature, propQName, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE ) );
  }

  // TODO implements this constructor
  /**
   * Creates a feature with this gml id. This
   */
  public FE1D2DNode( final Feature parentFeature, final QName propQName, final String gmlID )
  {
    this( FeatureHelper.createFeatureWithId( Kalypso1D2DSchemaConstants.WB1D2D_F_NODE, parentFeature, propQName, gmlID ) );
  }

  public GM_Point getPoint( )
  {
    return (GM_Point) getFeature().getProperty( IFE1D2DNode.WB1D2D_PROP_POINT );
  }

  public void setPoint( final GM_Point point )
  {
    if( point.getCoordinateSystem() == null )
      point.setCoordinateSystem( IFE1D2DNode.DEFAULT_COORDINATE_SYSTEM );
    getFeature().setProperty( IFE1D2DNode.WB1D2D_PROP_POINT, point );
    // if(point.getCoordinateDimension()>2 || !Double.isNaN( point.getZ() ))
    // getWrappedFeature().setProperty( IFE1D2DNode.PROP_HAS_ELEVATION, true );
    // else
    // getWrappedFeature().setProperty( IFE1D2DNode.PROP_HAS_ELEVATION, false );
  }

  public static FE1D2DNode createNode( final IFEDiscretisationModel1d2d discModel )
  {
    final Feature parentFeature = discModel.getFeature();
    final IFeatureType parentFT = parentFeature.getFeatureType();
    final IRelationType parentNodeProperty = (IRelationType) parentFT.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_NODES );
    final IFeatureType nodeType = parentFT.getGMLSchema().getFeatureType( Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    final Feature nodeFeature = parentFeature.getWorkspace().createFeature( parentFeature, parentNodeProperty, nodeType );
    return new FE1D2DNode( nodeFeature );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFENode#getContainers()
   */
  public IFeatureWrapperCollection getContainers( )
  {
    return m_containers;
  }

  /**
   * Returns all elements, this node is part of.
   */
  public IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>[] getElements( )
  {
    // REMARK: method reworked to use references instead of geometry (by Dejan)
    // (we had Splitsort problem before: sometimes the search query in SplitSort does not return anything)
    final List<IFE1D2DElement> elementsList = new ArrayList<IFE1D2DElement>();
    for( final Object element : m_containers )
    {
      if( element instanceof IFE1D2DElement )
      {
        if( !elementsList.contains( element ) )
          elementsList.add( (IFE1D2DElement) element );
      }
      else if( element instanceof IFE1D2DEdge )
      {
        final IFE1D2DEdge edge = (IFE1D2DEdge) element;
        final IFeatureWrapperCollection edgeContainers = edge.getContainers();
        for( final Object edgeContainer : edgeContainers )
        {
          if( edgeContainer instanceof IFE1D2DElement )
            if( !elementsList.contains( edgeContainer ) )
              elementsList.add( (IFE1D2DElement) edgeContainer );
        }
      }
    }
    return elementsList.toArray( new IFE1D2DElement[0] );

    // exTODO: at the moment, the elements are found via the geometric position, maybe change this later to
    // references via the containers.

    // final FE1D2DDiscretisationModel model = new FE1D2DDiscretisationModel(
    // getFeature().getWorkspace().getRootFeature() );
    // final FeatureList elementList = (FeatureList) model.getFeature().getProperty(
    // IFEDiscretisationModel1d2d.WB1D2D_PROP_ELEMENTS );
    //
    // // get all elements touching this node
    // // REMARK: for some reason sometimes the search query in SplitSort does not return anything.
    // final List touchingElements = elementList.query( getPoint().getPosition(), null );
    //
    // final List<IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>> foundElements = new
    // ArrayList<IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>>();
    //
    // // filter all element which contain this node
    // for( final Object object : touchingElements )
    // {
    // final Feature f = (Feature) object;
    // final IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge> elt = (IFE1D2DElement<IFE1D2DComplexElement,
    // IFE1D2DEdge>) f.getAdapter( IFE1D2DElement.class );
    // if( elt instanceof IElement1D )
    // {
    // // Special case for 1D-Elements, although they are elements. Its getEdges method is not supported.
    // final IElement1D elt1d = (IElement1D) elt;
    // final IFE1D2DEdge edge = elt1d.getEdge();
    // final IFeatureWrapperCollection nodes = edge.getNodes();
    // if( nodes.contains( this ) )
    // foundElements.add( elt );
    // }
    // else if( elt instanceof IElement2D )
    // {
    // final IFeatureWrapperCollection<IFE1D2DEdge> edges = ((IElement2D) elt).getEdges();
    // for( final IFE1D2DEdge edge : edges )
    // {
    // final IFeatureWrapperCollection nodes = edge.getNodes();
    // if( nodes.contains( this ) )
    // {
    // foundElements.add( elt );
    // // If one edge contains this node, we know the element contains it, so we can stop here
    // break;
    // }
    // }
    // }
    // }
    //
    // final IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>[] result = new IFE1D2DElement[foundElements.size()];
    // for( int i = 0; i < foundElements.size(); i++ )
    // result[i] = foundElements.get( i );
    //
    // return result;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode#getNeighbours()
   */
  public List<IFE1D2DNode> getNeighbours( )
  {
    final List<IFE1D2DNode> list = new ArrayList<IFE1D2DNode>();
    final IFeatureWrapperCollection nodeContainers = getContainers();
    for( final Object container : nodeContainers )
    {
      if( container instanceof IFE1D2DEdge )
      {
        final IFeatureWrapperCollection<IFE1D2DNode> nodes = ((IFE1D2DEdge) container).getNodes();
        for( final IFE1D2DNode node : nodes )
          if( !getGmlID().equals( node.getGmlID() ) )
            list.add( node );
      }
    }
    return list;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode#addContainer(java.lang.String)
   */
  public void addContainer( String linkRef )
  {
    linkRef = Assert.throwIAEOnNullOrEmpty( linkRef );
    final FeatureList wrappedList = m_containers.getWrappedList();
    if( !wrappedList.contains( linkRef ) )
      wrappedList.add( linkRef );
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final StringBuffer buf = new StringBuffer( 128 );
    buf.append( "FE1D2DNode" ); //$NON-NLS-1$
    buf.append( getGmlID() );
    buf.append( '[' );
    buf.append( getPoint() );
    // edges
    final IFeatureWrapperCollection containers = getContainers();
    buf.append( "{Edges=" ); //$NON-NLS-1$
    for( int i = 0; i < containers.size(); i++ )
      if( containers.get( i ) instanceof IFE1D2DEdge )
        buf.append( ((IFE1D2DEdge) containers.get( i )).getGmlID() );
    buf.append( '}' );
    buf.append( ']' );
    return buf.toString();
  }
}
