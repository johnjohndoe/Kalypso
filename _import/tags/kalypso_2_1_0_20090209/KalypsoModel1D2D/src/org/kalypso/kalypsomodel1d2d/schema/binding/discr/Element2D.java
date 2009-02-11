package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.Util;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Provide the default implementation for {@link org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEElement}.
 * Those classes can be used as java abstract for the substituable of wb1d2d:FE1D2D_2DElement:
 * 
 * 
 * @author Gernot Belger, Patrice Congo
 */
@SuppressWarnings("hiding")
public abstract class Element2D<CT extends IFE1D2DComplexElement, ET extends IFE1D2DEdge> extends FE1D2DElement<CT, ET> implements IElement2D<CT, ET>
{
  private final IFeatureWrapperCollection<ET> edges;
  public final static QName WB1D2D_F_FE1D2D_2DElement = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "Element2D" );

  /**
   * Creates an element with a specified GML ID. The parent feature respectively its link to the newly created element
   * the the actual feature q-name and the binding classes for edges and container complex elements are specified as
   * parameters.
   * 
   * @param parentFeature
   *            the parent feature
   * @param propQName
   *            the q-name of the property linking the parent feature to the continuity line
   * @param gmlID
   *            the gml id for the newly created feature
   * @param featureQName
   *            the q-name of the feature to create
   * @param complexElementClass
   *            the target binding class for container complex elements
   * @param edgeClass
   *            the target binding class for the edges
   * @throws IllegalArgumentException
   *             if any argument is null
   */
  public Element2D( final Feature parentFeature, final QName propQName, final String gmlID, final QName featureQName, final Class<CT> complexElementClass, final Class<ET> edgeClass )
  {
    this( FeatureHelper.createFeatureWithId( featureQName, parentFeature, propQName, gmlID ), featureQName, complexElementClass, edgeClass );
  }

  /**
   * Creates a new element binding the specified feature
   * 
   * @param featureToBind
   *            the feature to bind
   * @param featureQName
   *            the required feature q-name
   * @param complexElementClass
   *            the target binding class for containing complex element
   * @param edgeClass
   *            the target binding class for edges
   */
  public Element2D( final Feature featureToBind, final QName featureQName, final Class<CT> complexElementClass, final Class<ET> edgeClass )
  {
    super( featureToBind, featureQName, complexElementClass );
    Object prop = null;
    try
    {
      prop = featureToBind.getProperty( IFE1D2DElement.WB1D2D_PROP_ELEMENT_CONTAINERS );
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
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
      edges = new FeatureWrapperCollection<ET>( featureToBind,
      // TODO: problem here?
      Element2D.WB1D2D_F_FE1D2D_2DElement, FE1D2DElement.WB1D2D_PROP_DIRECTEDEDGE, edgeClass// IFE1D2DEdge.class
      );
    }
    else
    {
      // just wrapped the existing one
      edges = new FeatureWrapperCollection<ET>( featureToBind, edgeClass, FE1D2DElement.WB1D2D_PROP_DIRECTEDEDGE );
    }
  }

  public Element2D( final Feature featureToBind )
  {
    this( featureToBind, Element2D.WB1D2D_F_FE1D2D_2DElement );
  }

  public Element2D( final Feature featureToBind, final QName featureQName )
  {
    this( featureToBind, featureQName, (Class<CT>) IFEComplexElement2D.class, (Class<ET>) IFE1D2DEdge.class );
  }

  /**
   * This constructor creates {@link FE1D2D_2DElement} based on a feature which is created as child of the given parent
   * feaure and linked to it by the property of the type specified by the argument propQName. The Type of the feature is
   * also specified by the given element.
   * 
   * @param parentFeature
   *            the parent feature for the new wbr:Roughness class
   * @param propQName
   *            the Q-name of the linking property type
   * @param newFeatureQName
   *            the Q-Name denoting the type of the new feature
   * @throws IllegalArgumentException
   *             if workspace is null or the roughness collection is not part of the workspace
   */
  public Element2D( final Feature parentFeature, final QName propQName, final QName newFeatureQName ) throws IllegalArgumentException
  {
    this( Util.createFeatureAsProperty( parentFeature, propQName, newFeatureQName ) );
  }

  public Element2D( final Feature parentFeature, final QName propQName, final String gmlID )
  {
    this( FeatureHelper.createFeatureWithId( Element2D.WB1D2D_F_FE1D2D_2DElement, parentFeature, propQName, gmlID ) );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement2D#getGeometry()
   */
  public GM_Surface<GM_SurfacePatch> getGeometry( )
  {
    return getProperty( QNAME_PROP_GEOMETRY, GM_Surface.class );
  }

  /**
   * Returns the (dereferenced) nodes of this egde. Elements of the array may be null.
   */
  public FE1D2DEdge[] getEdgesAsArray( )
  {
    final Feature feature = getFeature();
    final GMLWorkspace workspace = feature.getWorkspace();
    final List edgeList = (List) feature.getProperty( FE1D2DElement.WB1D2D_PROP_DIRECTEDEDGE );

    final FE1D2DEdge[] edges = new FE1D2DEdge[edgeList.size()];
    for( int i = 0; i < edges.length; i++ )
    {
      /*
       * Accessing the list via index is ok here, because we should never have edges with more than 2 nodes.
       */
      final String ref = (String) edgeList.get( i );
      if( ref == null )
        edges[i] = null;
      else
        edges[i] = new FE1D2DEdge( workspace.getFeature( ref ) );
    }

    return edges;

    // return edges.toArray( new FE1D2DEdge[edges.size()] );

  }

  @SuppressWarnings("unchecked")
  public void setEdges( final ET[] edges )
  {
    final Feature feature = getFeature();
    final List edgeList = (List) feature.getProperty( FE1D2DElement.WB1D2D_PROP_DIRECTEDEDGE );

    /*
     * remove former edges and un register this as container
     */
    for( final ET edge : edges )
    {
      edge.removeContainerAsRef( this );
    }
    edgeList.clear();

    // Feature edgeInvFeature;

    for( final IFE1D2DEdge edge : edges )
    {
      edgeList.add( edge.getGmlID() );
    }
    // ModelOps.sortElementEdges( this );

    getFeature().invalidEnvelope();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEElement#getEdges()
   */
  public IFeatureWrapperCollection<ET> getEdges( )
  {
    return edges;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement#getNodes()
   */
  @Override
  public List<IFE1D2DNode> getNodes( )
  {

    final List<IFE1D2DNode> nodes = new ArrayList<IFE1D2DNode>( edges.size() + 1 );
    IFE1D2DNode lasAddedNode = null;

    for( final IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge : edges )
    {
      for( final IFE1D2DNode node : edge.getNodes() )
      {
        if( node != null )
        {
          if( !node.equals( lasAddedNode ) )
          {
            nodes.add( node );
            lasAddedNode = node;
          }
        }
      }
    }
    if( lasAddedNode != null && nodes.size() > 0 )
    {
      if( !lasAddedNode.equals( nodes.get( 0 ) ) )
      {
        nodes.add( nodes.get( 0 ) );
      }
    }
    return nodes;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement#addEdge(java.lang.String)
   */
  public void addEdge( final String edgeID )
  {
    if( edgeID == null )
      throw new IllegalArgumentException( Messages.getString( "Element2D.2" ) ); //$NON-NLS-1$
    final FeatureList edgeFeatureList = edges.getWrappedList();
    if( edgeFeatureList.contains( edgeID ) )
      return;
    edgeFeatureList.add( edgeID );
    edgeFeatureList.invalidate();
    getFeature().invalidEnvelope();
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final StringBuffer buf = new StringBuffer( 128 );
    buf.append( "Element2D[" ); //$NON-NLS-1$
    for( final IFeatureWrapper2 featureWrapper : edges )
    {
      buf.append( featureWrapper );
    }
    buf.append( ']' );
    return buf.toString();
  }
}
