package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * The default implementation of {@link IFE1D2DNode} based on {@link Feature_Impl} to bind wb1d2d:Node elements
 * 
 * @author Gernot Belger, Patrice Congo
 */
public class FE1D2DNode extends Feature_Impl implements IFE1D2DNode
{
  public FE1D2DNode( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  private FeatureList linkedItemsInternal( )
  {
    return (FeatureList)getProperty( MEMBER_NODE_CONTAINERS );
  }

  @Override
  public GM_Point getPoint( )
  {
    return (GM_Point)getProperty( IFE1D2DNode.PROPERTY_POINT );
  }

  @Override
  public void setPoint( final GM_Point point )
  {
    Assert.throwIAEOnNullParam( point, "point" ); //$NON-NLS-1$
    Assert.throwIAEOnNull( point.getCoordinateSystem(), "Point must have a coordinate system." ); //$NON-NLS-1$
    setProperty( IFE1D2DNode.PROPERTY_POINT, point );
  }

  @Override
  public IFE1D2DEdge[] getLinkedEdges( )
  {
    final FeatureList linkedItemsInternal = linkedItemsInternal();
    final Feature[] features = linkedItemsInternal.toFeatures( new Feature[linkedItemsInternal.size()] );
    final List<IFE1D2DEdge> results = new ArrayList<>( linkedItemsInternal.size() );
    for( final Feature item : features )
    {
      if( item instanceof IFE1D2DEdge )
        results.add( (IFE1D2DEdge)item );
    }
    return results.toArray( new IFE1D2DEdge[results.size()] );
  }

  /**
   * Returns all elements, this node is part of.
   */
  @Override
  public IFE1D2DElement[] getAdjacentElements( )
  {
    final List<IFE1D2DElement> result = new ArrayList<>();
    for( final IFE1D2DEdge edge : getLinkedEdges() )
    {
      final IFE1D2DElement[] elements = edge.getLinkedElements();
      for( final IFE1D2DElement element : elements )
      {
        if( !result.contains( element ) )
          result.add( element );
      }
    }
    return result.toArray( new IFE1D2DElement[result.size()] );
  }

  @Override
  public List<IFE1D2DNode> getAdjacentNodes( )
  {
    final List<IFE1D2DNode> result = new ArrayList<>();
    final IFE1D2DEdge[] edges = getLinkedEdges();
    for( final IFE1D2DEdge edge : edges )
    {
      final IFE1D2DNode[] nodes = edge.getNodes();
      for( final IFE1D2DNode node : nodes )
        if( !this.equals( node ) )
          result.add( node );
    }
    return result;
  }

  @Override
  public void addLinkedEdge( final IFE1D2DEdge edge )
  {
    Assert.throwIAEOnNullParam( edge, "edge" ); //$NON-NLS-1$
    if( !linkedItemsInternal().containsLinkTo( edge ) )
      linkedItemsInternal().addLink( edge );
  }

  @Override
  public void removeLinkedEdge( final IFE1D2DEdge edge )
  {
    Assert.throwIAEOnNullParam( edge, "edge" ); //$NON-NLS-1$
    if( linkedItemsInternal().containsLinkTo( edge ) )
      linkedItemsInternal().removeLink( edge );
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
    final Feature[] containers = getLinkedEdges();
    buf.append( "{Edges=" ); //$NON-NLS-1$
    for( final Feature container : containers )
      buf.append( ((IFE1D2DEdge)container).getId() );
    buf.append( '}' );
    buf.append( ']' );
    return buf.toString();
  }

  @Override
  public boolean isAdjacentNode( final IFE1D2DNode node )
  {
    final List<IFE1D2DNode> adjacentNodes = getAdjacentNodes();
    return adjacentNodes.contains( node );
  }
}