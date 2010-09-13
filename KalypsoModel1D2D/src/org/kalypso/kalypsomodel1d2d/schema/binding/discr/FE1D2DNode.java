package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

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

  @Override
  public GM_Point getPoint( )
  {
    return (GM_Point) getFeature().getProperty( IFE1D2DNode.WB1D2D_PROP_POINT );
  }

  @Override
  public void setPoint( final GM_Point point )
  {
    if( point.getCoordinateSystem() == null )
      throw new IllegalStateException();
    
    getFeature().setProperty( IFE1D2DNode.WB1D2D_PROP_POINT, point );
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
  @Override
  public IFeatureWrapperCollection getContainers( )
  {
    return m_containers;
  }

  /**
   * Returns all elements, this node is part of.
   */
  @Override
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
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode#getNeighbours()
   */
  @Override
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
  @Override
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
