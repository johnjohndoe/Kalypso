package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * The default implementation of {@link IFE1D2DNode} based on {@link Feature_Impl} to bind wb1d2d:Node elements
 *
 * @author Gernot Belger, Patrice Congo
 */
public class FE1D2DNode extends Feature_Impl implements IFE1D2DNode
{
  /** Edges that contains this node */
  private final IFeatureBindingCollection<IFENetItem> m_containers = new FeatureBindingCollection<>( this, IFENetItem.class, MEMBER_NODE_CONTAINERS );

  public FE1D2DNode( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  @Override
  public GM_Point getPoint( )
  {
    return (GM_Point)getProperty( IFE1D2DNode.PROPERTY_POINT );
  }

  @Override
  public void setPoint( final GM_Point point )
  {
    if( point.getCoordinateSystem() == null )
      throw new IllegalStateException();

    setProperty( IFE1D2DNode.PROPERTY_POINT, point );
  }

  @Override
  public IFeatureBindingCollection<IFENetItem> getContainers( )
  {
    return m_containers;
  }

  /**
   * Returns all elements, this node is part of.
   */
  @Override
  public IFE1D2DElement[] getElements( )
  {
    // REMARK: method reworked to use references instead of geometry (by Dejan)
    // (we had Splitsort problem before: sometimes the search query in SplitSort does not return anything)
    final List<IFE1D2DElement> elementsList = new ArrayList<>();
    for( final Object element : m_containers )
    {
      if( element instanceof IFE1D2DElement )
      {
        if( !elementsList.contains( element ) )
          elementsList.add( (IFE1D2DElement)element );
      }
      else if( element instanceof IFE1D2DEdge )
      {
        final IFE1D2DEdge edge = (IFE1D2DEdge)element;
        final IFeatureBindingCollection<IFE1D2DElement> edgeContainers = edge.getContainers();
        for( final IFE1D2DElement edgeContainer : edgeContainers )
        {
          if( !elementsList.contains( edgeContainer ) )
            elementsList.add( edgeContainer );
        }
      }
    }
    return elementsList.toArray( new IFE1D2DElement[0] );
  }

  @Override
  public List<IFE1D2DNode> getNeighbours( )
  {
    final List<IFE1D2DNode> list = new ArrayList<>();
    final IFeatureBindingCollection<IFENetItem> nodeContainers = getContainers();
    for( final Object container : nodeContainers )
    {
      if( container instanceof IFE1D2DEdge )
      {
        final IFeatureBindingCollection<IFE1D2DNode> nodes = ((IFE1D2DEdge)container).getNodes();
        for( final IFE1D2DNode node : nodes )
          if( !getId().equals( node.getId() ) )
            list.add( node );
      }
    }
    return list;
  }

  @Override
  public void addContainer( String linkRef )
  {
    linkRef = Assert.throwIAEOnNullOrEmpty( linkRef );
    final FeatureList wrappedList = m_containers.getFeatureList();
    if( !wrappedList.contains( linkRef ) )
      wrappedList.add( linkRef );
  }

  @Override
  public String toString( )
  {
    final StringBuffer buf = new StringBuffer( 128 );
    buf.append( "FE1D2DNode" ); //$NON-NLS-1$
    buf.append( getId() );
    buf.append( '[' );
    buf.append( getPoint() );
    // edges
    final IFeatureBindingCollection<IFENetItem> containers = getContainers();
    buf.append( "{Edges=" ); //$NON-NLS-1$
    for( int i = 0; i < containers.size(); i++ )
      if( containers.get( i ) instanceof IFE1D2DEdge )
        buf.append( ((IFE1D2DEdge)containers.get( i )).getId() );
    buf.append( '}' );
    buf.append( ']' );
    return buf.toString();
  }

  @Override
  public GM_Object recalculateElementGeometry( )
  {
    return null;
  }
}