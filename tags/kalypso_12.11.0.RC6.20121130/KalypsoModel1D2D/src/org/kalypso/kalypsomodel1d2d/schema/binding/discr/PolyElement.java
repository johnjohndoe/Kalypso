package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Polygon;

/**
 * @author Gernot Belger, Patrice Congo
 */
public class PolyElement extends FE1D2DElement implements IPolyElement
{
  public PolyElement( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  private FeatureList edgesInternal( )
  {
    return (FeatureList)getProperty( WB1D2D_PROP_DIRECTEDEDGE );
  }

  @Override
  public GM_Polygon getGeometry( )
  {
    return getProperty( QNAME_PROP_GEOMETRY, GM_Polygon.class );
  }

  @Override
  public void setEdges( final IFE1D2DEdge[] edges )
  {
    Assert.throwIAEOnNull( edges, "edge ID must not be null" ); //$NON-NLS-1$
//    if( edges.length < 3 || edges.length > 4 )
//      throw new IllegalStateException( String.format( "Cannot create an element with %d edges.", edges.length ) );

    final IFE1D2DEdge[] currentEdges = getEdges();
    for( final IFE1D2DEdge edge : currentEdges )
    {
      edge.removeLinkedElement( this );
    }

    final FeatureList edgesInternal = edgesInternal();
    edgesInternal.clear();

    for( final IFE1D2DEdge edge : edges )
    {
      edgesInternal.addLink( edge );
      edge.addLinkedElement( this );
    }

    setEnvelopesUpdated();
  }

  @Override
  public IFE1D2DEdge[] getEdges( )
  {
    return edgesInternal().toFeatures( new IFE1D2DEdge[edgesInternal().size()] );
  }

  @Override
  public IFE1D2DNode[] getNodes( )
  {
    final IFE1D2DEdge[] edges = getEdges();
    if( edges.length == 0 )
      return new IFE1D2DNode[0];

    final List<IFE1D2DNode> nodes = new ArrayList<>( edges.length + 1 );
    final IFE1D2DEdge firstEdge = edges[0];

    if( firstEdge == null )
    {
      System.out.println( "Corrupt mesh!" ); //$NON-NLS-1$
      return new IFE1D2DNode[0];
    }

    final IFE1D2DNode[] firstTwoNodes = firstEdge.getNodes();
    final IFE1D2DNode firstNode = firstTwoNodes[0];
    nodes.add( firstNode );

    final IFE1D2DNode secondNode = firstTwoNodes[1];
    IFE1D2DNode lastAddedNode = secondNode;

    while( lastAddedNode != null && !lastAddedNode.equals( firstNode ) )
    {
      nodes.add( lastAddedNode );
      lastAddedNode = getAdjacentNode( lastAddedNode, edges, nodes );
    }

    nodes.add( firstNode );

    return nodes.toArray( new IFE1D2DNode[nodes.size()] );
  }

  private IFE1D2DNode getAdjacentNode( final IFE1D2DNode node, final IFE1D2DEdge[] edges, final List<IFE1D2DNode> excludeNodes )
  {
    for( final IFE1D2DEdge edge : edges )
    {
      /* protect against corrupt net */
      if( edge == null )
      {
        System.out.println( "Corrupt mesh!" ); //$NON-NLS-1$
        continue;
      }

      final IFE1D2DNode[] nodes = edge.getNodes();
      final IFE1D2DNode firstNode = nodes[0];
      final IFE1D2DNode secondNode = nodes[1];
      if( firstNode.equals( node ) && !excludeNodes.contains( secondNode ) )
        return secondNode;
      if( secondNode.equals( node ) && !excludeNodes.contains( firstNode ) )
        return firstNode;
    }
    return null;
  }

  @Override
  public String toString( )
  {
    final StringBuffer buf = new StringBuffer( 128 );
    buf.append( "Poly_ELEMENT[" ); //$NON-NLS-1$
    for( final Object featureWrapper : edgesInternal() )
    {
      buf.append( featureWrapper );
    }
    buf.append( ']' );
    buf.append( getId() );
    return buf.toString();
  }

  @Override
  public String getRoughnessClsID( )
  {
    return getProperty( IFE1D2DElement.PROP_ROUGHNESS_CLS_ID, String.class );
  }

  @Override
  public Double getRoughnessCorrectionAxAy( )
  {
    return (Double)getProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_AXAY );
  }

  @Override
  public Double getRoughnessCorrectionDP( )
  {
    return (Double)getProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_DP );
  }

  @Override
  public Double getRoughnessCorrectionKS( )
  {
    return (Double)getProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_KS );
  }

  @Override
  public String getRoughnessStyle( )
  {
    final Object property = getProperty( IFE1D2DElement.PROP_ROUGHNESS_STYLE );
    if( property == null || property.toString().length() == 0 )
      return IRoughnessPolygon.NO_ROUGHNESS;
    return property.toString();
  }

  @Override
  public void setRoughnessClsID( final String value )
  {
    setProperty( IFE1D2DElement.PROP_ROUGHNESS_CLS_ID, value );
  }

  @Override
  public void setRoughnessCorrectionAxAy( final Double value )
  {
    setProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_AXAY, value );
  }

  @Override
  public void setRoughnessCorrectionDP( final Double value )
  {
    setProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_DP, value );
  }

  @Override
  public void setRoughnessCorrectionKS( final Double value )
  {
    setProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_KS, value );
  }

  @Override
  public void setRoughnessStyle( final String value )
  {
    setProperty( IFE1D2DElement.PROP_ROUGHNESS_STYLE, value );
  }

  @Override
  public boolean containsEdge( final IFE1D2DEdge edge )
  {
    return edgesInternal().containsOrLinksTo( edge );
  }
}