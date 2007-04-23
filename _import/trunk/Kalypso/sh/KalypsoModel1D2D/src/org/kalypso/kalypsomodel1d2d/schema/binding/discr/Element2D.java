package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.ops.ModelOps;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.Util;
import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper;

/**
 * Provide the default implementation for 
 * {@link org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEElement}.
 * Those classes kann be used as java abtract for the subtituable of 
 * wb1d2d:FE1D2D_2DElement:  wb1d2d:FE1D2DQuadriElement, 
 *  wb1d2d:FE1D2DTriElement and 
 * wb1d2d:FE1D2DContinuityLine 
 *  
 * 
 * @author Gernot Belger, Patrice Congo
 * @see IFE1D2DContinuityLine
 * @see FE1D2DContinuityLine
 */
@SuppressWarnings("hiding")
public abstract class Element2D<    
                          CT extends IFE1D2DComplexElement, 
                          ET extends IFE1D2DEdge>
                extends FE1D2DElement<CT,ET> 
                implements IElement2D<CT,ET>
{
  private final IFeatureWrapperCollection<ET> edges;

  public Element2D( 
      final Feature featureToBind, 
      QName featureQName,
      Class<CT> complexElementClass,
      Class<ET> edgeClass)
  {
    super(featureToBind, featureQName, complexElementClass); 
    Object prop =null;
    try
    {
        prop=featureToBind.getProperty( 
            Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENT_CONTAINERS );
    }
    catch(Throwable th)
    {
      th.printStackTrace();
    }

    // edges
    try
    {
      prop = featureToBind.getProperty( 
                  Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDEDGE );
    }
    catch (Throwable th) 
    {
      th.printStackTrace();
      prop=null;
    }

    if( prop == null )
    {
      // create the property that is still missing
      edges = new FeatureWrapperCollection<ET>( 
                          featureToBind, 
                          // TODO: problem here?
                          Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2D_2DElement, 
                          Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDEDGE, 
                          edgeClass//IFE1D2DEdge.class 
                          );
    }
    else
    {
      // just wrapped the existing one
      edges = 
        new FeatureWrapperCollection<ET>( 
                  featureToBind, 
                  edgeClass,//IFE1D2DEdge.class,// <IFE1D2DElement,IFE1D2DNode<IFE1D2DEdge>>.class,
                  Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDEDGE );
    }
  }
  
  public Element2D( final Feature featureToBind )
  {
    this(featureToBind, Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2D_2DElement );
  }
  
  public Element2D( 
            final Feature featureToBind, 
            final QName featureQName )
  {
    this( 
          featureToBind, 
          featureQName,
          (Class<CT>)IFEComplexElement2D.class,
          (Class<ET>)IFE1D2DEdge.class );
  }

  /**
   * This constructor creates {@link FE1D2D_2DElement} based on a
   * feature which is created as child of the given parent feaure and 
   * linked to it by the property of the type specified by the 
   * argument propQName. The Type of the feature is also specified by the given
   * element.
   * This constructor is typicaly used to construct feature like 
   * wb1d2d:FE1D2DQuadriElement, wb1d2d:FE1D2DTriElement and 
   * wb1d2d:FE1D2DContinuityLine 
   * 
   * @param parentFeature the parent feature for the new wbr:Roughness class
   * @param propQName  the Q-name of the linking property type
   * @param newFeatureQName the Q-Name denoting the type of the
   *            new feature
   * @throws IllegalArgumentException if workspace is null
   *  or the roughness collection is not part of the workspace
   */
  public Element2D(
        Feature parentFeature,
        QName propQName,
        QName newFeatureQName
        )
        throws IllegalArgumentException
  {
    this(Util.createFeatureAsProperty(
                parentFeature, 
                propQName,
                newFeatureQName));   
  }
  
  public Element2D(
      Feature parentFeature,
      QName propQName,
      String gmlID)
  {
    this(
      org.kalypso.kalypsosimulationmodel.core.Util.createFeatureWithId( 
          Kalypso1D2DSchemaConstants.WB1D2D_F_FE1D2D_2DElement,
          parentFeature, 
          propQName, 
          gmlID ));
  }
  
  /**
   * Returns the (dereferenced) nodes of this egde. Elements of the array may be null.
   */
  public FE1D2DEdge[] getEdgesAsArray( )
  {
    final Feature feature = getFeature();
    final GMLWorkspace workspace = feature.getWorkspace();
    final List edgeList = 
        (List) feature.getProperty( 
                Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDEDGE );

    
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
    
//    return edges.toArray( new FE1D2DEdge[edges.size()] );
    
  }

  @SuppressWarnings("unchecked")
  public void setEdges( final ET[] edges )
  {
    final Feature feature = getFeature();
    final List edgeList = (List) feature.getProperty( Kalypso1D2DSchemaConstants.WB1D2D_PROP_DIRECTEDEDGE );

    /*
     * remove former edges and un register this as container
     */
    for(ET edge:edges)
    {
      edge.removeContainerAsRef( this );
    }
    edgeList.clear();
    
//    Feature edgeInvFeature;
    
    for( final IFE1D2DEdge edge : edges )
    {
      edgeList.add( edge.getGmlID() );
    }
    ModelOps.sortElementEdges( this );
    
    getWrappedFeature().invalidEnvelope();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEElement#getEdges()
   */
  public IFeatureWrapperCollection<ET> getEdges( )
  {
    return (IFeatureWrapperCollection<ET>) edges;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement#getNodes()
   */
  @Override
  public List<IFE1D2DNode> getNodes( )
  {
    
    List<IFE1D2DNode> nodes= new ArrayList<IFE1D2DNode>(edges.size()+1);
    IFE1D2DNode lasAddedNode=null;
//    IFE1D2DNode actualNode;
//    IFE1D2DNode nextNode;
    
    for(IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode> edge:edges)
    {
      if(edge instanceof IEdgeInv)
      {
        IFE1D2DEdge invertedEdge=((IEdgeInv)edge).getInverted();
        List<IFE1D2DNode> edgeNodes=invertedEdge.getNodes();
        IFE1D2DNode<IFE1D2DEdge> node;
        for(int i=edgeNodes.size()-1;i>=0;i--)
        {
          node=edgeNodes.get( i );
          if(node!=null)
          {
            if(!node.equals( lasAddedNode ))
            {
              nodes.add( node );
              lasAddedNode=node;
            }
          }
        }
        
      }
      else
      {
        for(IFE1D2DNode<IFE1D2DEdge> node : edge.getNodes())
        {
          if(node!=null)
          {
            if(!node.equals( lasAddedNode ))
            {
              nodes.add( node );
              lasAddedNode=node;
            }
          }
        }
      }
    }
    if(lasAddedNode!=null && nodes.size()>0)
    {
      if(!lasAddedNode.equals( nodes.get( 0 ) ))
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
    if(edgeID==null)
    {
      throw new IllegalArgumentException("edge ID must not be null");
    }
    FeatureList edgeFeatureList=edges.getWrappedList();
    if(edgeFeatureList.contains( edgeID ))
    {
      return;
    }
    edgeFeatureList.add( edgeID );
    
    getWrappedFeature().invalidEnvelope();
  }
  
  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    StringBuffer buf= new StringBuffer(128);
    buf.append( "Element2D[" );
    for(IFeatureWrapper featureWrapper:edges)
    {
      buf.append( featureWrapper );
    }
    buf.append( ']' );
    return buf.toString();
  }
}
