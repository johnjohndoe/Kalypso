package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.geom.ModelGeometryBuilder;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;

/**
 * TODO: also make interface for this element
 *
 * Provide the default implementation for {@link org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEElement}.
 * Those classes kann be used as java abtract for the subtituable of wb1d2d:FE1D2D_2DElement:
 * wb1d2d:FE1D2DQuadriElement, wb1d2d:FE1D2DTriElement and wb1d2d:FE1D2DContinuityLine
 *
 *
 * @author Gernot Belger, Patrice Congo
 * @see IFE1D2DContinuityLine
 * @see FE1D2DContinuityLine
 */
public class PolyElement extends FE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge> implements IPolyElement<IFE1D2DComplexElement, IFE1D2DEdge>
{
  private final IFeatureWrapperCollection<IFE1D2DComplexElement> m_containers;

  private final IFeatureWrapperCollection<IFE1D2DEdge> m_edges;

  public PolyElement( final Feature featureToBind )
  {
    this( featureToBind, IPolyElement.QNAME );
  }

  public PolyElement( final Feature featureToBind, final QName featureQName )
  {
    super( featureToBind, featureQName, IFE1D2DComplexElement.class );
    //
    Object prop = null;
    try
    {
      prop = featureToBind.getProperty( IFE1D2DElement.WB1D2D_PROP_ELEMENT_CONTAINERS );
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
    }

    if( prop == null )
    {
      // create the property tha is still missing
      // TODO: remove this stuff
      m_containers = new FeatureWrapperCollection<IFE1D2DComplexElement>( featureToBind, IFE1D2DElement.WB1D2D_PROP_ELEMENT_CONTAINERS, IFE1D2DElement.WB1D2D_PROP_ELEMENT_CONTAINERS, IFE1D2DComplexElement.class );
    }
    else
    {
      // just wrapped the existing one
      m_containers = new FeatureWrapperCollection<IFE1D2DComplexElement>( featureToBind, IFE1D2DComplexElement.class, IFE1D2DElement.WB1D2D_PROP_ELEMENT_CONTAINERS );
    }

    // edges
    try
    {
      prop = featureToBind.getProperty( FE1D2DElement.WB1D2D_PROP_DIRECTEDEDGE );
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      prop = null;
    }

    if( prop == null )
    {
      // create the property that is still missing
      m_edges = new FeatureWrapperCollection<IFE1D2DEdge>( featureToBind, IPolyElement.QNAME, FE1D2DElement.WB1D2D_PROP_DIRECTEDEDGE, IFE1D2DEdge.class );
    }
    else
    {
      // just wrapped the existing one
      m_edges = new FeatureWrapperCollection<IFE1D2DEdge>( featureToBind, IFE1D2DEdge.class, FE1D2DElement.WB1D2D_PROP_DIRECTEDEDGE );
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement2D#getGeometry()
   */
  @Override
  public GM_Surface<GM_SurfacePatch> getGeometry( )
  {
    return getProperty( QNAME_PROP_GEOMETRY, GM_Surface.class );
  }

  @Override
  @SuppressWarnings("unchecked")
  public void setEdges( final IFE1D2DEdge[] edges )
  {
    final Feature feature = getFeature();
    final FeatureList edgeList = (FeatureList) feature.getProperty( FE1D2DElement.WB1D2D_PROP_DIRECTEDEDGE );

    edgeList.clear();

    for( final IFE1D2DEdge edge : edges )
    {
      edgeList.add( edge.getGmlID() );
    }

    // ModelOps.sortElementEdges( this );

    edgeList.invalidate();
    getFeature().invalidEnvelope();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement#addEdge(java.lang.String)
   */
  @Override
  public void addEdge( final String edgeID )
  {
    if( edgeID == null )
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.discr.Element2D.2" ) ); //$NON-NLS-1$
    final FeatureList edgeFeatureList = m_edges.getWrappedList();
    if( edgeFeatureList.contains( edgeID ) )
      return;
    edgeFeatureList.add( edgeID );
    edgeFeatureList.invalidate();
    getFeature().invalidEnvelope();
  }

  /**
   * Recalculates the geometry of this element. Used by the corresponding property function.
   */
  @Override
  public GM_Object recalculateElementGeometry( ) throws GM_Exception
  {
    return ModelGeometryBuilder.createSurfaceFromNode( getNodes() );
  }

  public static IPolyElement createPolyElement( final IFEDiscretisationModel1d2d discModel )
  {
    final Feature parentFeature = discModel.getFeature();
    final IFeatureType parentFT = parentFeature.getFeatureType();
    final IRelationType parentElementProperty = (IRelationType) parentFT.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_ELEMENTS );

    final IFeatureType polyType = parentFT.getGMLSchema().getFeatureType( IPolyElement.QNAME );

    final Feature eleFeature = parentFeature.getWorkspace().createFeature( parentFeature, parentElementProperty, polyType );

    return new PolyElement( eleFeature );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEElement#getContainers()
   */
  @Override
  public IFeatureWrapperCollection<IFE1D2DComplexElement> getContainers( )
  {
    return m_containers;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEElement#getEdges()
   */
  @Override
  public IFeatureWrapperCollection<IFE1D2DEdge> getEdges( )
  {
    return m_edges;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement#getNodes()
   */
  @Override
  public List<IFE1D2DNode> getNodes( )
  {
    final List<IFE1D2DNode> nodes = new ArrayList<IFE1D2DNode>( m_edges.size() + 1 );

    if( m_edges.size() < 3 )
      // this must be wrong
      return nodes;

    final IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> firstEdge = m_edges.get( 0 );
    final IFeatureWrapperCollection<IFE1D2DNode> firstTwoNodes = firstEdge.getNodes();
    final IFE1D2DNode firstNode = firstTwoNodes.get( 0 );
    nodes.add( firstNode );

    final IFE1D2DNode secondNode = firstTwoNodes.get( 1 );
    IFE1D2DNode lastAddedNode = secondNode;

    while( lastAddedNode != null && !lastAddedNode.equals( firstNode ) )
    {
      nodes.add( lastAddedNode );
      lastAddedNode = getAdjacentNode( lastAddedNode, nodes );
    }

    nodes.add( firstNode );

    if( nodes.size() < 4 )
      // this must be wrong
      return new ArrayList<IFE1D2DNode>( 0 );

    return nodes;
  }

  private IFE1D2DNode getAdjacentNode( final IFE1D2DNode node, final List<IFE1D2DNode> excludeNodes )
  {
    for( final IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge : m_edges )
    {
      final IFeatureWrapperCollection<IFE1D2DNode> nodes = edge.getNodes();
      final IFE1D2DNode firstNode = nodes.get( 0 );
      final IFE1D2DNode secondNode = nodes.get( 1 );

      if( firstNode == null )
        throw new IllegalStateException( Messages.getString("org.kalypso.kalypsomodel1d2d.schema.binding.discr.PolyElement.0") + edge.getGmlID() ); //$NON-NLS-1$

      if( firstNode.equals( node ) && !excludeNodes.contains( secondNode ) )
        return secondNode;

      if( secondNode == null )
        throw new IllegalStateException( Messages.getString("org.kalypso.kalypsomodel1d2d.schema.binding.discr.PolyElement.1") + edge.getGmlID() ); //$NON-NLS-1$

      if( secondNode.equals( node ) && !excludeNodes.contains( firstNode ) )
        return firstNode;
    }
    return null;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final StringBuffer buf = new StringBuffer( 128 );
    buf.append( "Poly_ELEMENT[" ); //$NON-NLS-1$
    for( final IFeatureWrapper2 featureWrapper : m_edges )
    {
      buf.append( featureWrapper );
    }
    buf.append( ']' );
    buf.append( getFeature().getId() );
    return buf.toString();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#getRoughnessClsID()
   */
  @Override
  public String getRoughnessClsID( )
  {
    final Object property = getFeature().getProperty( IFE1D2DElement.PROP_ROUGHNESS_CLS_ID );
    if( property == null )
      return ""; //$NON-NLS-1$
    return property.toString();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#getRoughnessCorrectionAxAy()
   */
  @Override
  public Double getRoughnessCorrectionAxAy( )
  {
    return (Double) getFeature().getProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_AXAY );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#getRoughnessCorrectionDP()
   */
  @Override
  public Double getRoughnessCorrectionDP( )
  {
    return (Double) getFeature().getProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_DP );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#getRoughnessCorrectionKS()
   */
  @Override
  public Double getRoughnessCorrectionKS( )
  {
    return (Double) getFeature().getProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_KS );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#getRoughnessStyle()
   */
  @Override
  public String getRoughnessStyle( )
  {
    final Object property = getFeature().getProperty( IFE1D2DElement.PROP_ROUGHNESS_STYLE );
    if( property == null || property.toString().length() == 0 )
      return IRoughnessPolygon.NO_ROUGHNESS;
    return property.toString();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#setRoughnessClsID(java.lang.String)
   */
  @Override
  public void setRoughnessClsID( final String value )
  {
    getFeature().setProperty( IFE1D2DElement.PROP_ROUGHNESS_CLS_ID, value );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#setRoughnessCorrectionAxAy(java.lang.String)
   */
  @Override
  public void setRoughnessCorrectionAxAy( final Double value )
  {
    getFeature().setProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_AXAY, value );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#setRoughnessCorrectionDP(java.lang.String)
   */
  @Override
  public void setRoughnessCorrectionDP( final Double value )
  {
    getFeature().setProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_DP, value );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#setRoughnessCorrectionKS(java.lang.String)
   */
  @Override
  public void setRoughnessCorrectionKS( final Double value )
  {
    getFeature().setProperty( IFE1D2DElement.PROP_ROUGHNESS_CORRECTION_KS, value );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement#setRoughnessStyle(java.lang.String)
   */
  @Override
  public void setRoughnessStyle( final String value )
  {
    getFeature().setProperty( IFE1D2DElement.PROP_ROUGHNESS_STYLE, value );
  }

}
