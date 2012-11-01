/**
 *
 */
package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Gernot Belger
 */
public class FE1D2DEdge extends Feature_Impl implements IFE1D2DEdge
{
  public FE1D2DEdge( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  private FeatureList elementsInternal( )
  {
    return (FeatureList)getProperty( WB1D2D_PROP_EDGE_CONTAINERS );
  }

  private FeatureList nodesInternal( )
  {
    return (FeatureList)getProperty( WB1D2D_PROP_DIRECTEDNODE );
  }

  @Override
  public IFE1D2DNode[] getNodes( )
  {
    return nodesInternal().toFeatures( new IFE1D2DNode[nodesInternal().size()] );
  }

  @Override
  public void setNodes( final IFE1D2DNode node0, final IFE1D2DNode node1 )
  {
    Assert.throwIAEOnNullParam( node0, "node0" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( node1, "node1" ); //$NON-NLS-1$
    nodesInternal().clear();
    nodesInternal().addLink( node0 );
    nodesInternal().addLink( node1 );
    node0.addLinkedEdge( this );
    node1.addLinkedEdge( this );
  }

  @Override
  public void addLinkedElement( final IFE1D2DElement element )
  {
    Assert.throwIAEOnNullParam( element, "element" ); //$NON-NLS-1$
    if( !elementsInternal().containsOrLinksTo( element ) )
      elementsInternal().addLink( element );
  }

  @Override
  public void removeLinkedElement( final IFE1D2DElement element )
  {
    Assert.throwIAEOnNullParam( element, "element" ); //$NON-NLS-1$
    if( elementsInternal().containsOrLinksTo( element ) )
      elementsInternal().removeLink( element );
  }

  @Override
  public GM_Point getMiddleNodePoint( )
  {
    Feature[] nodes = getNodes();
    final GM_Point point1 = ((IFE1D2DNode)nodes[0]).getPoint();
    final GM_Point point2 = ((IFE1D2DNode)nodes[1]).getPoint();
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

  @Override
  public IFE1D2DElement[] getLinkedElements( )
  {
    return elementsInternal().toFeatures( new IFE1D2DElement[elementsInternal().size()] );
  }

  @Override
  public boolean isBorder( )
  {
    return elementsInternal().size() == 1;
  }

  @Override
  public GM_Curve getGeometry( )
  {
    return getProperty( WB1D2D_PROP_MIDDLE_GEOM, GM_Curve.class );
  }

  @Override
  public boolean containsNode( final IFE1D2DNode node )
  {
    Assert.throwIAEOnNullParam( node, "node" ); //$NON-NLS-1$
    return nodesInternal().containsOrLinksTo( node );
  }

  @Override
  public String toString( )
  {
    final StringBuffer buf = new StringBuffer( 256 );
    buf.append( this.getClass().getSimpleName() );
    buf.append( '[' );
    for( final Feature node : getNodes() )
    {
      buf.append( node );
      buf.append( ' ' );
    }
    buf.append( ']' );
    return buf.toString();
  }
}